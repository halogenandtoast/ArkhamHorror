module Arkham.EnemyLocation.Cards.LivingBedroomHemlockHouse35 (livingBedroomHemlockHouse35) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Matcher

newtype LivingBedroomHemlockHouse35 = LivingBedroomHemlockHouse35 EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingBedroomHemlockHouse35 :: EnemyLocationCard LivingBedroomHemlockHouse35
livingBedroomHemlockHouse35 =
  enemyLocationWith
    LivingBedroomHemlockHouse35
    Cards.livingBedroomHemlockHouse35
    (2, StaticWithPerPlayer 3 2, 3)
    (1, 1)
    (\la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 3)}})

instance HasAbilities LivingBedroomHemlockHouse35 where
  getAbilities (LivingBedroomHemlockHouse35 a) =
    getAbilities a
      <> [ -- "Forced - When this enemy-location is revealed: It attacks each
           -- investigator at this location."
           mkAbility a 1
             $ forced
             $ FlipLocation #after Anyone (LocationWithId a.id)
         ]

instance RunMessage LivingBedroomHemlockHouse35 where
  runMessage msg el@(LivingBedroomHemlockHouse35 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ Do EnemiesAttack
      pure el
    _ -> LivingBedroomHemlockHouse35 <$> liftRunMessage msg attrs
