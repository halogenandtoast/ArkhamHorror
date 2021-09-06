module Arkham.Types.Enemy.Cards.DevoteeOfTheKey
  ( devoteeOfTheKey
  , DevoteeOfTheKey(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Phase
import qualified Arkham.Types.Timing as Timing

newtype DevoteeOfTheKey = DevoteeOfTheKey EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
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
  => RunMessage env DevoteeOfTheKey where
  runMessage msg e@(DevoteeOfTheKey attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      leadInvestigatorId <- getLeadInvestigatorId
      sentinelPeak <- fromJustNote "missing location"
        <$> selectOne (LocationWithTitle "Sentinel Peak")
      if enemyLocation == sentinelPeak
        then
          e <$ pushAll
            [Discard (toTarget attrs), PlaceDoomOnAgenda, PlaceDoomOnAgenda]
        else do
          choices <- map unClosestPathLocationId
            <$> getSetList (enemyLocation, sentinelPeak, emptyLocationMap)
          case choices of
            [] -> error "should not happen"
            [x] -> e <$ push (EnemyMove enemyId enemyLocation x)
            xs ->
              e
                <$ push
                     (chooseOne leadInvestigatorId
                     $ map (EnemyMove enemyId enemyLocation) xs
                     )
    _ -> DevoteeOfTheKey <$> runMessage msg attrs
