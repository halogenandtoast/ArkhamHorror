module Arkham.EnemyLocation.Cards.LivingLibraryHemlockHouse40 (livingLibraryHemlockHouse40) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Matcher

newtype LivingLibraryHemlockHouse40 = LivingLibraryHemlockHouse40 EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingLibraryHemlockHouse40 :: EnemyLocationCard LivingLibraryHemlockHouse40
livingLibraryHemlockHouse40 =
  enemyLocationWith
    LivingLibraryHemlockHouse40
    Cards.livingLibraryHemlockHouse40
    (2, StaticWithPerPlayer 4 1, 3)
    (1, 1)
    (\la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 3)}})

instance HasAbilities LivingLibraryHemlockHouse40 where
  getAbilities (LivingLibraryHemlockHouse40 a) =
    getAbilities a
      <> [ -- "Forced - When this enemy-location is revealed: Each investigator
           -- at this location and each connecting location takes 1 horror."
           mkAbility a 1
             $ forced
             $ FlipLocation #after Anyone (LocationWithId a.id)
         ]

instance RunMessage LivingLibraryHemlockHouse40 where
  runMessage msg el@(LivingLibraryHemlockHouse40 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      iids <- select $ InvestigatorAt $ orConnected_ (LocationWithId attrs.id)
      for_ iids $ \iid -> assignHorror iid (attrs.ability 1) 1
      pure el
    _ -> LivingLibraryHemlockHouse40 <$> liftRunMessage msg attrs
