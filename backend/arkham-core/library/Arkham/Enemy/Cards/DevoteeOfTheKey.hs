module Arkham.Enemy.Cards.DevoteeOfTheKey
  ( devoteeOfTheKey
  , DevoteeOfTheKey(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Projection
import Arkham.Timing qualified as Timing

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

instance RunMessage DevoteeOfTheKey where
  runMessage msg e@(DevoteeOfTheKey attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      enemyLocation <- field EnemyLocation enemyId
      for_ enemyLocation $ \loc -> do
        leadInvestigatorId <- getLeadInvestigatorId
        sentinelPeak <- selectJust (LocationWithTitle "Sentinel Peak")
        if loc == sentinelPeak
          then pushAll
            [Discard (toTarget attrs), PlaceDoomOnAgenda, PlaceDoomOnAgenda]
          else do
            choices <- selectList $ ClosestPathLocation loc sentinelPeak
            case choices of
              [] -> error "should not happen"
              [x] -> push (EnemyMove enemyId x)
              xs -> push $ chooseOne
                leadInvestigatorId
                [ targetLabel x [EnemyMove enemyId x] | x <- xs ]
      pure e
    _ -> DevoteeOfTheKey <$> runMessage msg attrs
