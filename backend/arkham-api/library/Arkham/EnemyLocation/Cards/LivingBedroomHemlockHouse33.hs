module Arkham.EnemyLocation.Cards.LivingBedroomHemlockHouse33 (livingBedroomHemlockHouse33) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Matcher

newtype LivingBedroomHemlockHouse33 = LivingBedroomHemlockHouse33 EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingBedroomHemlockHouse33 :: EnemyLocationCard LivingBedroomHemlockHouse33
livingBedroomHemlockHouse33 =
  enemyLocationWith
    LivingBedroomHemlockHouse33
    Cards.livingBedroomHemlockHouse33
    (2, StaticWithPerPlayer 3 2, 3)
    (1, 1)
    (\la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 3)}})

instance HasAbilities LivingBedroomHemlockHouse33 where
  getAbilities (LivingBedroomHemlockHouse33 a) =
    getAbilities a
      <> [ -- "Forced - When this enemy-location is revealed: It attacks each
           -- investigator at this location."
           mkAbility a 1
             $ forced
             $ FlipLocation #after Anyone (LocationWithId a.id)
         ]

instance RunMessage LivingBedroomHemlockHouse33 where
  runMessage msg el@(LivingBedroomHemlockHouse33 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ Do EnemiesAttack
      pure el
    _ -> LivingBedroomHemlockHouse33 <$> liftRunMessage msg attrs
