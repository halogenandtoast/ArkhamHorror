module Arkham.EnemyLocation.Cards.LivingParlorHemlockHouse (livingParlorHemlockHouse) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype LivingParlorHemlockHouse = LivingParlorHemlockHouse EnemyLocationAttrs
  deriving anyclass IsEnemyLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingParlorHemlockHouse :: EnemyLocationCard LivingParlorHemlockHouse
livingParlorHemlockHouse =
  enemyLocationWith
    LivingParlorHemlockHouse
    Cards.livingParlorHemlockHouse
    (3, Static 3, 1)
    (1, 2)
    \la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 4)}}

instance HasModifiersFor LivingParlorHemlockHouse where
  getModifiersFor (LivingParlorHemlockHouse a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance HasAbilities LivingParlorHemlockHouse where
  getAbilities (LivingParlorHemlockHouse a) =
    extend1 a $ mkAbility a 1 $ forced $ FlipLocation #after Anyone (LocationWithId a.id)

instance RunMessage LivingParlorHemlockHouse where
  runMessage msg el@(LivingParlorHemlockHouse attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      sendMessage attrs $ Do EnemiesAttack
      pure el
    _ -> LivingParlorHemlockHouse <$> liftRunMessage msg attrs
