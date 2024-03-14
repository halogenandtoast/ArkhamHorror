module Arkham.Enemy.Cards.MoonBeast (moonBeast, MoonBeast (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers

newtype MoonBeast = MoonBeast EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonBeast :: EnemyCard MoonBeast
moonBeast = enemy MoonBeast Cards.moonBeast (5, Static 5, 1) (1, 1)

instance HasAbilities MoonBeast where
  getAbilities (MoonBeast a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemySpawns #after Anywhere $ be a
      , mkAbility a 2 $ forced $ EnemyDefeated #after You ByAny $ be a
      ]

instance RunMessage MoonBeast where
  runMessage msg e@(MoonBeast attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator (raiseAlarmLevel (attrs.ability 1))
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      eachInvestigator (reduceAlarmLevel (attrs.ability 2))
      pure e
    _ -> MoonBeast <$> lift (runMessage msg attrs)
