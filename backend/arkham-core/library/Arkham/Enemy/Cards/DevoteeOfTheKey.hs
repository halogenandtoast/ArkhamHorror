module Arkham.Enemy.Cards.DevoteeOfTheKey
  ( devoteeOfTheKey
  , DevoteeOfTheKey(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import qualified Arkham.Enemy.Cards as Cards
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import qualified Arkham.Timing as Timing

newtype DevoteeOfTheKey = DevoteeOfTheKey EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devoteeOfTheKey :: EnemyCard DevoteeOfTheKey
devoteeOfTheKey = enemyWith
  DevoteeOfTheKey
  Cards.devoteeOfTheKey
  (3, Static 3, 3)
  (1, 1)
  (spawnAtL ?~ LocationWithTitle "Base of the Hill")

instance HasAbilities DevoteeOfTheKey where
  getAbilities (DevoteeOfTheKey attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1 $ ForcedAbility $ PhaseEnds Timing.When $ PhaseIs
        EnemyPhase
    ]

instance
  ( Query LocationMatcher env
  , EnemyRunner env
  )
  => RunMessage DevoteeOfTheKey where
  runMessage msg e@(DevoteeOfTheKey attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      case enemyLocation of
        Nothing -> pure e
        Just loc -> do
          leadInvestigatorId <- getLeadInvestigatorId
          sentinelPeak <- selectJust (LocationWithTitle "Sentinel Peak")
          if loc == sentinelPeak
            then
              e <$ pushAll
                [Discard (toTarget attrs), PlaceDoomOnAgenda, PlaceDoomOnAgenda]
            else do
              choices <- map unClosestPathLocationId
                <$> getSetList (loc, sentinelPeak, emptyLocationMap)
              case choices of
                [] -> error "should not happen"
                [x] -> e <$ push (EnemyMove enemyId x)
                xs ->
                  e
                    <$ push
                         (chooseOne leadInvestigatorId
                         $ map (EnemyMove enemyId) xs
                         )
    _ -> DevoteeOfTheKey <$> runMessage msg attrs
