module Arkham.Location.Cards.TracesToNowhere.MerchantDistrict (merchantDistrict) where

import Arkham.Location.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TracesToNowhere.Helpers

newtype MerchantDistrict = MerchantDistrict LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

merchantDistrict :: LocationCard MerchantDistrict
merchantDistrict = location MerchantDistrict Cards.merchantDistrict 3 (PerPlayer 1)

instance HasAbilities MerchantDistrict where
  getAbilities (MerchantDistrict a) = extendRevealed1 a $ hireACab a 1

instance RunMessage MerchantDistrict where
  runMessage msg l@(MerchantDistrict attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hireACabEffect iid (attrs.ability 1)
      pure l
    _ -> MerchantDistrict <$> liftRunMessage msg attrs
