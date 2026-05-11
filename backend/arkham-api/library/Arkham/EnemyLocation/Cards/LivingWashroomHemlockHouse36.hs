module Arkham.EnemyLocation.Cards.LivingWashroomHemlockHouse36 (livingWashroomHemlockHouse36) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Matcher

newtype LivingWashroomHemlockHouse36 = LivingWashroomHemlockHouse36 EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingWashroomHemlockHouse36 :: EnemyLocationCard LivingWashroomHemlockHouse36
livingWashroomHemlockHouse36 =
  enemyLocationWith
    LivingWashroomHemlockHouse36
    Cards.livingWashroomHemlockHouse36
    (2, Static 3, 3)
    (1, 1)
    (\la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 4)}})

instance HasAbilities LivingWashroomHemlockHouse36 where
  getAbilities (LivingWashroomHemlockHouse36 a) =
    getAbilities a
      <> [ -- "Forced - When this enemy-location is revealed, if there are 1 or
           -- more clues on it: It attacks each investigator at this location."
           restricted a 1 (LocationExists (LocationWithId a.id <> LocationWithAnyClues))
             $ forced
             $ FlipLocation #after Anyone (LocationWithId a.id)
         ]

instance RunMessage LivingWashroomHemlockHouse36 where
  runMessage msg el@(LivingWashroomHemlockHouse36 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ Do EnemiesAttack
      pure el
    _ -> LivingWashroomHemlockHouse36 <$> liftRunMessage msg attrs
