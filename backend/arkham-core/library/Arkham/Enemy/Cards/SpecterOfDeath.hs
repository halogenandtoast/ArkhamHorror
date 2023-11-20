module Arkham.Enemy.Cards.SpecterOfDeath (
  specterOfDeath,
  SpecterOfDeath (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Scenarios.ThePallidMask.Helpers
import Arkham.Timing qualified as Timing

newtype SpecterOfDeath = SpecterOfDeath EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

specterOfDeath :: EnemyCard SpecterOfDeath
specterOfDeath =
  enemyWith
    SpecterOfDeath
    Cards.specterOfDeath
    (0, Static 1, 0)
    (0, 0)
    (spawnAtL ?~ SpawnAt (LocationWithLabel $ positionToLabel startPosition))

instance HasAbilities SpecterOfDeath where
  getAbilities (SpecterOfDeath a) =
    withbaseAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ SkillTestResult
            Timing.After
            You
            (WhileEvadingAnEnemy $ EnemyWithId $ toId a)
            (FailureResult AnyValue)
      ]

instance RunMessage SpecterOfDeath where
  runMessage msg e@(SpecterOfDeath attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ EnemyAttack
        $ (enemyAttack (toId attrs) attrs iid) {attackDamageStrategy = enemyDamageStrategy attrs}
      pure e
    _ -> SpecterOfDeath <$> runMessage msg attrs
