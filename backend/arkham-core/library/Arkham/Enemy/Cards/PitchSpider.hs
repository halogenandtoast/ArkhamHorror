module Arkham.Enemy.Cards.PitchSpider (pitchSpider, PitchSpider (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey

newtype PitchSpider = PitchSpider EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pitchSpider :: EnemyCard PitchSpider
pitchSpider =
  enemyWith
    PitchSpider
    Cards.pitchSpider
    (2, Static 1, 4)
    (1, 1)
    $ spawnAtL
    ?~ SpawnAtFirst
      [ SpawnAt $ LocationWithTitle "Sea of Pitch" <> EmptyLocation
      , SpawnAt $ LocationWithTitle "Sea of Pitch"
      ]

instance HasModifiersFor PitchSpider where
  getModifiersFor target (PitchSpider a) | a `is` target = do
    x <- scenarioCount Distortion
    pure $ toModifiers a $ AttackDealsEitherDamageOrHorror : [SwarmingValue x | x > 0]
  getModifiersFor _ _ = pure []

instance HasAbilities PitchSpider where
  getAbilities (PitchSpider x) =
    extend
      x
      [ mkAbility x 1
          $ forced
          $ EnemyAttacks #when You AnyEnemyAttack (be x)
      ]

instance RunMessage PitchSpider where
  runMessage msg e@(PitchSpider attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pure e
    _ -> PitchSpider <$> runMessage msg attrs
