module Arkham.Location.Cards.ReturnToMerchantDistrict (returnToMerchantDistrict) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToMerchantDistrict = ReturnToMerchantDistrict LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToMerchantDistrict :: LocationCard ReturnToMerchantDistrict
returnToMerchantDistrict = location ReturnToMerchantDistrict Cards.returnToMerchantDistrict 4 (Static 0)

instance HasAbilities ReturnToMerchantDistrict where
  getAbilities (ReturnToMerchantDistrict attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToMerchantDistrict where
  runMessage msg (ReturnToMerchantDistrict attrs) = runQueueT $ case msg of
    _ -> ReturnToMerchantDistrict <$> liftRunMessage msg attrs
