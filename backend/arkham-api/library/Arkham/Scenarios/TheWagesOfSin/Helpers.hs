module Arkham.Scenarios.TheWagesOfSin.Helpers
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Classes.RunMessage.Internal
import Arkham.Enemy.Runner
import Arkham.Helpers
import Arkham.Id
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenario.Types (Field (..))
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Spectral))
import Control.Lens (non, _1, _2)
import Control.Monad.Writer.Class
import Data.Map.Monoidal.Strict (MonoidalMap)

getSpectralDiscards :: HasGame m => m [EncounterCard]
getSpectralDiscards =
  scenarioFieldMap ScenarioEncounterDecks (view (at SpectralEncounterDeck . non (Deck [], []) . _2))

getSpectralDeck :: HasGame m => m (Deck EncounterCard)
getSpectralDeck =
  scenarioFieldMap ScenarioEncounterDecks (view (at SpectralEncounterDeck . non (Deck [], []) . _1))

-- TODO: before the heretic spawns we don't know if it's going to be at a spectral or non-spectral location
-- This causes an issue when we flip unfinished business back over but the location is spectral
-- we might want spawning to be two steps, spawn at location, then engage
hereticModifiers
  :: ( EntityId (EntityAttrs a) ~ EnemyId
     , Targetable (EntityAttrs a)
     , HasGame m
     , Entity a
     , Entity (EntityAttrs a)
     , Sourceable (EntityAttrs a)
     , MonadWriter (MonoidalMap Target [Modifier]) m
     )
  => a
  -> m ()
hereticModifiers (toAttrs -> a) = do
  n <- perPlayer 2
  atSpectralLocation <- selectAny $ locationWithEnemy (toId a) <> LocationWithTrait Spectral
  modifySelf a
    $ HealthModifier n
    : ( guard (not atSpectralLocation)
          *> [AddKeyword Aloof, CannotBeDamaged, CannotBeEngaged]
      )

hereticAbilities
  :: ( EntityId (EntityAttrs a) ~ EnemyId
     , HasAbilities (EntityAttrs a)
     , Sourceable (EntityAttrs a)
     , HasCardCode (EntityAttrs a)
     , Entity a
     , Entity (EntityAttrs a)
     )
  => a
  -> [Ability]
hereticAbilities (toAttrs -> a) =
  withBaseAbilities
    a
    [ restrictedAbility a 1 OnSameLocation $ FastAbility' (ClueCost $ Static 1) [#parley]
    , mkAbility a 2 $ ForcedAbility $ EnemyDefeated Timing.After Anyone ByAny $ EnemyWithId $ toId a
    ]

hereticRunner
  :: ( IsEnemy b
     , HasCardCode storyCard
     )
  => storyCard
  -> Runner b
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
