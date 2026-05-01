module Arkham.Location.Cards.MerchantDistrict_c2026 (merchantDistrict_c2026) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.GameValue
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Location.Cards qualified as Cards (merchantDistrict_c2026)
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype MerchantDistrict_c2026 = MerchantDistrict_c2026 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

merchantDistrict_c2026 :: LocationCard MerchantDistrict_c2026
merchantDistrict_c2026 = location MerchantDistrict_c2026 Cards.merchantDistrict_c2026 3 (PerPlayer 1)

instance HasAbilities MerchantDistrict_c2026 where
  getAbilities (MerchantDistrict_c2026 a) =
    extendRevealed1 a $ groupLimit PerRound $ restricted a 1 restriction $ FastAbility' Free #move
   where
    restriction =
      Here
        <> oneOf (map PlayerCountIs [1, 2])
        <> DuringTurn You
        <> CanMoveTo (ConnectedLocation ForMovement)

instance RunMessage MerchantDistrict_c2026 where
  runMessage msg l@(MerchantDistrict_c2026 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
      pure l
    _ -> MerchantDistrict_c2026 <$> liftRunMessage msg attrs
