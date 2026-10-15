module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Enemies.ServantOfTwoMasters (servantOfTwoMasters) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype ServantOfTwoMasters = ServantOfTwoMasters EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfTwoMasters :: EnemyCard ServantOfTwoMasters
servantOfTwoMasters = enemy ServantOfTwoMasters Cards.servantOfTwoMasters

instance HasAbilities ServantOfTwoMasters where
  getAbilities (ServantOfTwoMasters a) =
    extend
      a
      [ restricted a 1 CanPlaceDoomOnThis $ forced $ PhaseEnds #when #mythos
      , restricted a 2 (thisExists a $ EnemyWithDoom $ atLeast 1)
          $ forced
          $ EnemyWouldBeDefeated #when (be a)
      ]

instance RunMessage ServantOfTwoMasters where
  runMessage msg e@(ServantOfTwoMasters attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      removeDoom (attrs.ability 2) attrs 1
      healAllDamage (attrs.ability 2) attrs
      cancelEnemyDefeat attrs
      pure e
    _ -> ServantOfTwoMasters <$> liftRunMessage msg attrs
