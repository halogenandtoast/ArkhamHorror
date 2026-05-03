module Arkham.Enemy.Cards.DarkMagician (darkMagician) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype DarkMagician = DarkMagician EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkMagician :: EnemyCard DarkMagician
darkMagician = enemy DarkMagician Cards.darkMagician (4, Static 4, 2) (1, 1)

instance HasAbilities DarkMagician where
  getAbilities (DarkMagician a) =
    extend1 a
      $ restricted
        a
        1
        ( exists
            $ NearestEnemyToLocationMatch
              (locationWithEnemy a)
              (EnemyWithoutDoom <> CanPlaceDoomOnEnemy <> not_ (be a))
        )
      $ forced
      $ RoundEnds #when

instance RunMessage DarkMagician where
  runMessage msg e@(DarkMagician attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      enemies <-
        select
          $ NearestEnemyToLocationMatch (locationWithEnemy attrs)
          $ EnemyWithoutDoom
          <> CanPlaceDoomOnEnemy
          <> not_ (be attrs)
      chooseTargetM lead enemies $ placeDoomOn (attrs.ability 1) 1
      pure e
    _ -> DarkMagician <$> liftRunMessage msg attrs
