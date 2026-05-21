module Arkham.EnemyLocation.Cards.LivingLibraryHemlockHouse39 (livingLibraryHemlockHouse39) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype LivingLibraryHemlockHouse39 = LivingLibraryHemlockHouse39 EnemyLocationAttrs
  deriving anyclass IsEnemyLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingLibraryHemlockHouse39 :: EnemyLocationCard LivingLibraryHemlockHouse39
livingLibraryHemlockHouse39 =
  enemyLocationWith
    LivingLibraryHemlockHouse39
    Cards.livingLibraryHemlockHouse39
    (4, Static 4, 2)
    (0, 3)
    \la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 3)}}

instance HasModifiersFor LivingLibraryHemlockHouse39 where
  getModifiersFor (LivingLibraryHemlockHouse39 a) = do
    n <- perPlayer 1
    modifySelf a [HealthModifier n]

instance HasAbilities LivingLibraryHemlockHouse39 where
  getAbilities (LivingLibraryHemlockHouse39 a) =
    extend1 a $ mkAbility a 1 $ forced $ FlipLocation #after Anyone (LocationWithId a.id)

instance RunMessage LivingLibraryHemlockHouse39 where
  runMessage msg el@(LivingLibraryHemlockHouse39 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      iids <- select $ InvestigatorAt $ orConnected_ (LocationWithId attrs.id)
      for_ iids \iid -> loseResources iid (attrs.ability 1) 2
      pure el
    _ -> LivingLibraryHemlockHouse39 <$> liftRunMessage msg attrs
