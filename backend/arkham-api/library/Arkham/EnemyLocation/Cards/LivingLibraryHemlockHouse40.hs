module Arkham.EnemyLocation.Cards.LivingLibraryHemlockHouse40 (livingLibraryHemlockHouse40) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype LivingLibraryHemlockHouse40 = LivingLibraryHemlockHouse40 EnemyLocationAttrs
  deriving anyclass IsEnemyLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingLibraryHemlockHouse40 :: EnemyLocationCard LivingLibraryHemlockHouse40
livingLibraryHemlockHouse40 =
  enemyLocationWith
    LivingLibraryHemlockHouse40
    Cards.livingLibraryHemlockHouse40
    (4, Static 4, 2)
    (0, 3)
    \la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 3)}}

instance HasModifiersFor LivingLibraryHemlockHouse40 where
  getModifiersFor (LivingLibraryHemlockHouse40 a) = do
    n <- perPlayer 1
    modifySelf a [HealthModifier n]

instance HasAbilities LivingLibraryHemlockHouse40 where
  getAbilities (LivingLibraryHemlockHouse40 a) =
    extend1 a $ mkAbility a 1 $ forced $ FlipLocation #after Anyone (LocationWithId a.id)

instance RunMessage LivingLibraryHemlockHouse40 where
  runMessage msg el@(LivingLibraryHemlockHouse40 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      iids <- select $ InvestigatorAt $ orConnected_ (LocationWithId attrs.id)
      for_ iids \iid -> assignHorror iid (attrs.ability 1) 1
      pure el
    _ -> LivingLibraryHemlockHouse40 <$> liftRunMessage msg attrs
