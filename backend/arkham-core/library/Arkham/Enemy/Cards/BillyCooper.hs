module Arkham.Enemy.Cards.BillyCooper (billyCooper, BillyCooper (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait

newtype BillyCooper = BillyCooper EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

billyCooper :: EnemyCard BillyCooper
billyCooper = enemyWith BillyCooper Cards.billyCooper (5, Static 4, 2) (2, 0) (spawnAtL ?~ SpawnAt "Easttown")

instance HasAbilities BillyCooper where
  getAbilities (BillyCooper attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ forced
          $ defeated #after
          $ EnemyAt (locationWithEnemy $ toId attrs)
          <> withTrait Monster
      ]

instance RunMessage BillyCooper where
  runMessage msg e@(BillyCooper attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ addToVictory attrs
      pure e
    _ -> BillyCooper <$> runMessage msg attrs
