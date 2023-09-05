module Arkham.Location.Cards.MerchantDistrict_300
  ( merchantDistrict_300
  , MerchantDistrict_300(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MerchantDistrict_300 = MerchantDistrict_300 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

merchantDistrict_300 :: LocationCard MerchantDistrict_300
merchantDistrict_300 = location MerchantDistrict_300 Cards.merchantDistrict_300 2 (Static 0)

instance HasAbilities MerchantDistrict_300 where
  getAbilities (MerchantDistrict_300 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage MerchantDistrict_300 where
  runMessage msg (MerchantDistrict_300 attrs) =
    MerchantDistrict_300 <$> runMessage msg attrs
