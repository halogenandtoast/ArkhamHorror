module Arkham.EnemyLocation.Cards.LivingWashroomHemlockHouse37 (livingWashroomHemlockHouse37) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Matcher

newtype LivingWashroomHemlockHouse37 = LivingWashroomHemlockHouse37 EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingWashroomHemlockHouse37 :: EnemyLocationCard LivingWashroomHemlockHouse37
livingWashroomHemlockHouse37 =
  enemyLocationWith
    LivingWashroomHemlockHouse37
    Cards.livingWashroomHemlockHouse37
    (2, Static 3, 3)
    (1, 1)
    (\la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 4)}})

instance HasAbilities LivingWashroomHemlockHouse37 where
  getAbilities (LivingWashroomHemlockHouse37 a) =
    getAbilities a
      <> [ restricted a 1 (LocationExists (LocationWithId a.id <> LocationWithAnyClues))
             $ forced
             $ FlipLocation #after Anyone (LocationWithId a.id)
         ]

instance RunMessage LivingWashroomHemlockHouse37 where
  runMessage msg el@(LivingWashroomHemlockHouse37 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ Do EnemiesAttack
      pure el
    _ -> LivingWashroomHemlockHouse37 <$> liftRunMessage msg attrs
