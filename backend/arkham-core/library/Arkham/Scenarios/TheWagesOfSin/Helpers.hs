module Arkham.Scenarios.TheWagesOfSin.Helpers
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Classes.RunMessage.Internal
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Id
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Scenario.Deck
import Arkham.Scenario.Types (Field (..))
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Spectral))
import Control.Lens (non, _2)

getSpectralDiscards :: (HasGame m) => m [EncounterCard]
getSpectralDiscards =
  scenarioFieldMap ScenarioEncounterDecks (view (at SpectralEncounterDeck . non (Deck [], []) . _2))

hereticModifiers
  :: ( EntityId (EntityAttrs a) ~ EnemyId
     , Targetable (EntityAttrs a)
     , HasGame m
     , Entity a
     , Entity (EntityAttrs a)
     , Sourceable (EntityAttrs a)
     )
  => Target
  -> a
  -> m [Modifier]
hereticModifiers target (toAttrs -> a) | isTarget a target = do
  n <- perPlayer 2
  atSpectralLocation <-
    selectAny $ locationWithEnemy (toId a) <> LocationWithTrait Spectral
  pure $
    toModifiers a $
      HealthModifier n
        : if atSpectralLocation then [] else [AddKeyword Aloof, CannotBeDamaged, CannotBeEngaged]
hereticModifiers _ _ = pure []

hereticAbilities
  :: ( EntityId (EntityAttrs a) ~ EnemyId
     , HasAbilities (EntityAttrs a)
     , Sourceable (EntityAttrs a)
     , Entity a
     , Entity (EntityAttrs a)
     )
  => a
  -> [Ability]
hereticAbilities (toAttrs -> a) =
  withBaseAbilities
    a
    [ restrictedAbility a 1 OnSameLocation $ FastAbility $ ClueCost (Static 1)
    , mkAbility a 2 $ ForcedAbility $ EnemyDefeated Timing.After Anyone $ EnemyWithId $ toId a
    ]

hereticRunner
  :: ( Sourceable (EntityAttrs b)
     , Targetable (EntityAttrs b)
     , IsCard (EntityAttrs b)
     , Entity b
     , RunMessage (EntityAttrs b)
     , HasCardCode storyCard
     )
  => storyCard
  -> Message
  -> b
  -> GameT b
hereticRunner storyCard msg heretic = case msg of
  UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
    push $ LookAtRevealed iid (toSource attrs) (toTarget attrs)
    pure heretic
  UseCardAbility _iid (isSource attrs -> True) 2 _ _ -> do
    pure heretic
  Flip _ _ (isTarget attrs -> True) -> do
    pure heretic
  LookAtRevealed iid _ (isTarget attrs -> True) -> do
    let unfinishedBusiness = lookupCard storyCard (toCardId attrs)
    pushAll
      [ FocusCards [unfinishedBusiness]
      , chooseOne iid [Label "Continue" [UnfocusCards]]
      ]
    pure heretic
  _ -> overAttrsM (runMessage msg) heretic
 where
  attrs = toAttrs heretic
