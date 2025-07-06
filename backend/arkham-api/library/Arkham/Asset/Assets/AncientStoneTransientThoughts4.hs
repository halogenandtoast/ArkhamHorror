module Arkham.Asset.Assets.AncientStoneTransientThoughts4 (ancientStoneTransientThoughts4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Helpers.Location
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype AncientStoneTransientThoughts4 = AncientStoneTransientThoughts4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientStoneTransientThoughts4
  :: AssetCard AncientStoneTransientThoughts4
ancientStoneTransientThoughts4 = asset AncientStoneTransientThoughts4 Cards.ancientStoneTransientThoughts4

instance HasAbilities AncientStoneTransientThoughts4 where
  getAbilities (AncientStoneTransientThoughts4 a) =
    [ controlled a 1 (youExist can.move)
        $ triggered (DrawsCards #when You AnyCards AnyValue) (DynamicUseCost (be a) Secret DrawnCardsValue)
    ]

instance RunMessage AncientStoneTransientThoughts4 where
  runMessage msg a@(AncientStoneTransientThoughts4 attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 _ (totalUsesPayment -> n) -> do
      repeated n $ do_ msg
      pure a
    Do (UseThisAbility iid (isSource attrs -> True) 1) -> do
      xs <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid xs $ moveTo (attrs.ability 1) iid
      pure a
    _ -> AncientStoneTransientThoughts4 <$> liftRunMessage msg attrs
