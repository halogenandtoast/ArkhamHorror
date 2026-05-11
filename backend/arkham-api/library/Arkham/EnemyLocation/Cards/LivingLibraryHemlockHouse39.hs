module Arkham.EnemyLocation.Cards.LivingLibraryHemlockHouse39 (livingLibraryHemlockHouse39) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Matcher

newtype LivingLibraryHemlockHouse39 = LivingLibraryHemlockHouse39 EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- "Living Library gets +1 [per_investigator] health."
livingLibraryHemlockHouse39 :: EnemyLocationCard LivingLibraryHemlockHouse39
livingLibraryHemlockHouse39 =
  enemyLocationWith
    LivingLibraryHemlockHouse39
    Cards.livingLibraryHemlockHouse39
    (2, StaticWithPerPlayer 4 1, 3)
    (1, 1)
    (\la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 3)}})

instance HasAbilities LivingLibraryHemlockHouse39 where
  getAbilities (LivingLibraryHemlockHouse39 a) =
    getAbilities a
      <> [ -- "Forced - When this enemy-location is revealed: Each investigator
           -- at this location and each connecting location loses 2 resources."
           mkAbility a 1
             $ forced
             $ FlipLocation #after Anyone (LocationWithId a.id)
         ]

instance RunMessage LivingLibraryHemlockHouse39 where
  runMessage msg el@(LivingLibraryHemlockHouse39 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      iids <- select $ InvestigatorAt $ orConnected_ (LocationWithId attrs.id)
      for_ iids $ \iid -> loseResources iid (attrs.ability 1) 2
      pure el
    _ -> LivingLibraryHemlockHouse39 <$> liftRunMessage msg attrs
