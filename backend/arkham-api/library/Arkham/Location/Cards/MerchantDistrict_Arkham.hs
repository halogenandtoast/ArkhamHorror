{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.MerchantDistrict_Arkham (merchantDistrict_Arkham) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.GameValue
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Location.Cards qualified as Cards (merchantDistrict_Arkham)
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype MerchantDistrict_Arkham = MerchantDistrict_Arkham LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

merchantDistrict_Arkham :: LocationCard MerchantDistrict_Arkham
merchantDistrict_Arkham = location MerchantDistrict_Arkham Cards.merchantDistrict_Arkham 3 (PerPlayer 1)

instance HasAbilities MerchantDistrict_Arkham where
  getAbilities (MerchantDistrict_Arkham a) =
    extendRevealed1 a $ groupLimit PerRound $ restricted a 1 restriction $ FastAbility' Free #move
   where
    restriction =
      Here
        <> oneOf (map PlayerCountIs [1, 2])
        <> DuringTurn You
        <> CanMoveTo (ConnectedLocation ForMovement)

instance RunMessage MerchantDistrict_Arkham where
  runMessage msg l@(MerchantDistrict_Arkham attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
      pure l
    _ -> MerchantDistrict_Arkham <$> liftRunMessage msg attrs
