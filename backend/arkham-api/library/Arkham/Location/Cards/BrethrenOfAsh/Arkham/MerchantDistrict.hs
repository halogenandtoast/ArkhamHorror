{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.BrethrenOfAsh.Arkham.MerchantDistrict (merchantDistrict) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.GameValue
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Location.CardDefs.BrethrenOfAsh.Arkham qualified as Cards (merchantDistrict)
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype MerchantDistrict = MerchantDistrict LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

merchantDistrict :: LocationCard MerchantDistrict
merchantDistrict = location MerchantDistrict Cards.merchantDistrict 3 (PerPlayer 1)

instance HasAbilities MerchantDistrict where
  getAbilities (MerchantDistrict a) =
    extendRevealed1 a $ playerLimit PerRound $ restricted a 1 restriction $ FastAbility' Free #move
   where
    restriction =
      Here
        <> oneOf (map PlayerCountIs [1, 2])
        <> DuringTurn You
        <> CanMoveTo (ConnectedLocation ForMovement)

instance RunMessage MerchantDistrict where
  runMessage msg l@(MerchantDistrict attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
      pure l
    _ -> MerchantDistrict <$> liftRunMessage msg attrs
