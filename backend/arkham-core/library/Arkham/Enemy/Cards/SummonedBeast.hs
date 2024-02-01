module Arkham.Enemy.Cards.SummonedBeast (
  summonedBeast,
  SummonedBeast (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner hiding (EnemyEvade, EnemyFight)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Trait (Trait (Humanoid))

newtype SummonedBeast = SummonedBeast EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

summonedBeast :: EnemyCard SummonedBeast
summonedBeast = enemy SummonedBeast Cards.summonedBeast (5, PerPlayer 6, 2) (2, 2)

instance HasModifiersFor SummonedBeast where
  getModifiersFor target (SummonedBeast attrs) | isTarget attrs target = do
    n <- (`div` 2) <$> getDoomCount
    pure $ toModifiers attrs [EnemyFight n, EnemyEvade n, DamageDealt n, HorrorDealt n]
  getModifiersFor _ _ = pure []

instance HasAbilities SummonedBeast where
  getAbilities (SummonedBeast attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility
          attrs
          1
          (enemyExists $ EnemyAt (locationWithEnemy $ toId attrs) <> EnemyWithTrait Humanoid)
          $ ForcedAbility
          $ PhaseBegins #when #enemy
      , mkAbility attrs 2
          $ Objective
          $ ForcedAbility
          $ EnemyDefeated #when Anyone ByAny (EnemyWithId $ toId attrs)
      ]

instance RunMessage SummonedBeast where
  runMessage msg e@(SummonedBeast attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      humanoids <- selectList $ EnemyAt (locationWithEnemy $ toId attrs) <> EnemyWithTrait Humanoid
      doom <- selectSum EnemyDoom $ EnemyOneOf $ map EnemyWithId humanoids
      defeatMessages <- for humanoids $ \humanoid -> do
        defeatEnemy humanoid lead (toAbilitySource attrs 1)

      pushAll $ concat defeatMessages <> [placeDoom (toAbilitySource attrs 1) attrs doom]
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push R3
      pure e
    _ -> SummonedBeast <$> runMessage msg attrs
