module Arkham.EnemyLocation.Cards.LivingBedroomHemlockHouse35 (livingBedroomHemlockHouse35) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype LivingBedroomHemlockHouse35 = LivingBedroomHemlockHouse35 EnemyLocationAttrs
  deriving anyclass IsEnemyLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingBedroomHemlockHouse35 :: EnemyLocationCard LivingBedroomHemlockHouse35
livingBedroomHemlockHouse35 =
  enemyLocationWith
    LivingBedroomHemlockHouse35
    Cards.livingBedroomHemlockHouse35
    (3, Static 3, 2)
    (2, 0)
    \la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 3)}}

instance HasModifiersFor LivingBedroomHemlockHouse35 where
  getModifiersFor (LivingBedroomHemlockHouse35 a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance HasAbilities LivingBedroomHemlockHouse35 where
  getAbilities (LivingBedroomHemlockHouse35 a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ FlipLocation #after Anyone (LocationWithId a.id <> LocationWithInvestigator Anyone)

instance RunMessage LivingBedroomHemlockHouse35 where
  runMessage msg el@(LivingBedroomHemlockHouse35 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      sendMessage attrs $ Do EnemiesAttack
      pure el
    _ -> LivingBedroomHemlockHouse35 <$> liftRunMessage msg attrs
