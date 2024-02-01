module Arkham.Location.Cards.MerchantDistrict_301 (
  merchantDistrict_301,
  MerchantDistrict_301 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype MerchantDistrict_301 = MerchantDistrict_301 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

merchantDistrict_301 :: LocationCard MerchantDistrict_301
merchantDistrict_301 = location MerchantDistrict_301 Cards.merchantDistrict_301 3 (Static 0)

instance HasAbilities MerchantDistrict_301 where
  getAbilities (MerchantDistrict_301 attrs) =
    withRevealedAbilities attrs [fastAbility attrs 1 (ResourceCost 2) (withBreaches attrs Here)]

instance RunMessage MerchantDistrict_301 where
  runMessage msg l@(MerchantDistrict_301 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      act <- selectJust AnyAct
      pushAll [RemoveBreaches (toTarget attrs) 1, PlaceBreaches (toTarget act) 1]
      pure l
    _ -> MerchantDistrict_301 <$> runMessage msg attrs
