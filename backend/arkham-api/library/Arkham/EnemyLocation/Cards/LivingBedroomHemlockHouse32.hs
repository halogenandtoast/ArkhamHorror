module Arkham.EnemyLocation.Cards.LivingBedroomHemlockHouse32 (livingBedroomHemlockHouse32) where

import Arkham.Ability
import Arkham.Token
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Matcher

newtype LivingBedroomHemlockHouse32 = LivingBedroomHemlockHouse32 EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Stats: fight 2, health 3 + 2 per investigator, evade 3, 1 dmg / 1 horror
livingBedroomHemlockHouse32 :: EnemyLocationCard LivingBedroomHemlockHouse32
livingBedroomHemlockHouse32 =
  enemyLocationWith
    LivingBedroomHemlockHouse32
    Cards.livingBedroomHemlockHouse32
    (2, StaticWithPerPlayer 3 2, 3)
    (1, 1)
    \la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 3)}}

instance HasAbilities LivingBedroomHemlockHouse32 where
  getAbilities (LivingBedroomHemlockHouse32 a) =
     extend1 a $ mkAbility a 1 $ forced $ FlipLocation #after Anyone (LocationWithId a.id)

instance RunMessage LivingBedroomHemlockHouse32 where
  runMessage msg el@(LivingBedroomHemlockHouse32 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Doom 1
      pure el
    _ -> LivingBedroomHemlockHouse32 <$> liftRunMessage msg attrs
