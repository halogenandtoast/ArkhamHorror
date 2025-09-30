module Arkham.Location.Cards.MerchantDistrict_301 (merchantDistrict_301) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype MerchantDistrict_301 = MerchantDistrict_301 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

merchantDistrict_301 :: LocationCard MerchantDistrict_301
merchantDistrict_301 = location MerchantDistrict_301 Cards.merchantDistrict_301 3 (Static 0)

instance HasAbilities MerchantDistrict_301 where
  getAbilities (MerchantDistrict_301 a) =
    extendRevealed1 a $ fastAbility a 1 (ResourceCost 2) (withBreaches a Here)

instance RunMessage MerchantDistrict_301 where
  runMessage msg l@(MerchantDistrict_301 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      act <- selectJust AnyAct
      removeBreaches attrs 1
      placeBreaches act 1
      pure l
    _ -> MerchantDistrict_301 <$> liftRunMessage msg attrs
