module Arkham.Asset.Assets.MobConnections2 (mobConnections2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Window
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype MobConnections2 = MobConnections2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mobConnections2 :: AssetCard MobConnections2
mobConnections2 = asset MobConnections2 Cards.mobConnections2

instance HasAbilities MobConnections2 where
  getAbilities (MobConnections2 a) =
    [ controlled a 1 criteria $ actionAbilityWithCost (ResourceCost 2)
    , restricted a 2 ControlsThis
        $ triggered (AssetDiscarded #when $ #illicit <> AssetControlledBy You) (exhaust a)
    ]
   where
    criteria = if null a.cardsUnderneath then Never else youExist can.draw.cards

instance RunMessage MobConnections2 where
  runMessage msg a@(MobConnections2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      focusCards attrs.cardsUnderneath do
        chooseOrRunOneM iid $ targets attrs.cardsUnderneath (drawToHand iid . only)
      pure a
    UseCardAbility _iid (isSource attrs -> True) 2 (assetLeavingPlay -> aid) _ -> do
      card <- field AssetCard aid
      obtainCard card
      placeUnderneath attrs (only card)
      pure a
    _ -> MobConnections2 <$> liftRunMessage msg attrs
