module Arkham.Location.Cards.MerchantDistrict_301
  ( merchantDistrict_301
  , MerchantDistrict_301(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MerchantDistrict_301 = MerchantDistrict_301 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

merchantDistrict_301 :: LocationCard MerchantDistrict_301
merchantDistrict_301 = location MerchantDistrict_301 Cards.merchantDistrict_301 3 (Static 0)

instance HasAbilities MerchantDistrict_301 where
  getAbilities (MerchantDistrict_301 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage MerchantDistrict_301 where
  runMessage msg (MerchantDistrict_301 attrs) =
    MerchantDistrict_301 <$> runMessage msg attrs
