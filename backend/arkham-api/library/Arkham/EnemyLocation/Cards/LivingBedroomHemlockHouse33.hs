module Arkham.EnemyLocation.Cards.LivingBedroomHemlockHouse33 (livingBedroomHemlockHouse33) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype LivingBedroomHemlockHouse33 = LivingBedroomHemlockHouse33 EnemyLocationAttrs
  deriving anyclass IsEnemyLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingBedroomHemlockHouse33 :: EnemyLocationCard LivingBedroomHemlockHouse33
livingBedroomHemlockHouse33 =
  enemyLocationWith
    LivingBedroomHemlockHouse33
    Cards.livingBedroomHemlockHouse33
    (3, Static 3, 3)
    (1, 1)
    \la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 3)}}

instance HasModifiersFor LivingBedroomHemlockHouse33 where
  getModifiersFor (LivingBedroomHemlockHouse33 a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance HasAbilities LivingBedroomHemlockHouse33 where
  getAbilities (LivingBedroomHemlockHouse33 a) =
    extend1 a $ mkAbility a 1 $ forced $ FlipLocation #after Anyone (LocationWithId a.id)

instance RunMessage LivingBedroomHemlockHouse33 where
  runMessage msg el@(LivingBedroomHemlockHouse33 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      sendMessage attrs $ Do EnemiesAttack
      pure el
    _ -> LivingBedroomHemlockHouse33 <$> liftRunMessage msg attrs
