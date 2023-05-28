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
import Arkham.Enemy.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Id
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Scenario.Deck
import Arkham.Scenario.Types (Field (..))
import Arkham.Store
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Spectral))
import Control.Lens (non, _1, _2)

getSpectralDiscards :: (HasGame m, Store m Card) => m [EncounterCard]
getSpectralDiscards =
  scenarioFieldMap ScenarioEncounterDecks (view (at SpectralEncounterDeck . non (Deck [], []) . _2))

getSpectralDeck :: (HasGame m, Store m Card) => m (Deck EncounterCard)
getSpectralDeck =
  scenarioFieldMap ScenarioEncounterDecks (view (at SpectralEncounterDeck . non (Deck [], []) . _1))

-- TODO: before the heretic spawns we don't know if it's going to be at a spectral or non-spectral location
-- This causes an issue when we flip unfinished business back over but the location is spectral
-- we might want spawning to be two steps, spawn at location, then engage
hereticModifiers
  :: ( EntityId (EntityAttrs a) ~ EnemyId
     , Targetable (EntityAttrs a)
     , HasGame m
     , Store m Card
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
    , mkAbility a 2 $ ForcedAbility $ EnemyDefeated Timing.After Anyone ByAny $ EnemyWithId $ toId a
    ]

hereticRunner
  :: ( Sourceable (EntityAttrs b)
     , Targetable (EntityAttrs b)
     , IsEnemy b
     , RunMessage (EntityAttrs b)
     , HasCardCode storyCard
     )
  => storyCard
  -> Message
  -> b
  -> GameT b
hereticRunner storyCard msg heretic = case msg of
  UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
    let card = lookupCard storyCard (toCardId attrs)
    pushAll
      [ ReplaceCard (toCardId attrs) card
      , ReadStoryWithPlacement
          iid
          card
          DoNotResolveIt
          (Just $ toTarget $ toAttrs heretic)
          (enemyPlacement attrs)
      , ReplaceCard (toCardId attrs) card
      ]
    pure heretic
  UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
    push $ Flip iid (toSource attrs) (toTarget attrs)
    pure heretic
  Flip iid _ (isTarget attrs -> True) -> do
    let card = lookupCard storyCard (toCardId attrs)
    pushAll
      [ RemoveEnemy (toId attrs)
      , ReplaceCard (toCardId attrs) card
      , ReadStoryWithPlacement iid card ResolveIt Nothing (enemyPlacement attrs)
      ]
    pure heretic
  _ -> overAttrsM (runMessage msg) heretic
 where
  attrs = toAttrs heretic
