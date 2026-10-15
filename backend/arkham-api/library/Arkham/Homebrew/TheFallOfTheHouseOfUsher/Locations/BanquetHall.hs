module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.BanquetHall (banquetHall) where

import Arkham.Ability
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.Location.Import.Lifted

newtype BanquetHall = BanquetHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

banquetHall :: LocationCard BanquetHall
banquetHall = location BanquetHall Cards.banquetHall 2 (PerPlayer 1)

instance HasAbilities BanquetHall where
  getAbilities (BanquetHall a) =
    extendRevealed
      a
      [ scenarioI18n
          $ withI18nTooltip "banquetHall.hospitality"
          $ restricted a 1 Here
          $ actionAbilityWithCost
            (DamageCost (a.ability 1) YouTarget 1 <> HorrorCost (a.ability 1) YouTarget 1)
      , scenarioI18n $ collapseForced "banquetHall.collapse" a 2
      ]

instance RunMessage BanquetHall where
  runMessage msg l@(BanquetHall attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      recordHospitality iid
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      flipToRuined attrs
      pure l
    _ -> BanquetHall <$> liftRunMessage msg attrs
