module Arkham.Asset.Assets.SpiritSpeaker (spiritSpeaker) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher hiding (FastPlayerWindow)
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype SpiritSpeaker = SpiritSpeaker AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritSpeaker :: AssetCard SpiritSpeaker
spiritSpeaker = asset SpiritSpeaker Cards.spiritSpeaker

instance HasAbilities SpiritSpeaker where
  getAbilities (SpiritSpeaker a) =
    [ controlled a 1 (exists (AssetControlledBy You <> AssetWithUseType Charge)) (FastAbility $ exhaust a)
    ]

instance RunMessage SpiritSpeaker where
  runMessage msg a@(SpiritSpeaker attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assetIds <- select $ assetControlledBy iid <> AssetWithUseType Charge
      discardableAssetIds <- select $ assetControlledBy iid <> AssetWithUseType Charge <> DiscardableAsset
      assetIdsWithChargeCounts <- forToSnd assetIds $ fieldMap AssetUses (findWithDefault 0 Charge)
      chooseOneM iid do
        for_ assetIdsWithChargeCounts \(aid, n) -> do
          targeting aid do
            chooseOneM iid do
              labeled "Return to hand" $ returnToHand iid aid
              when (aid `elem` discardableAssetIds) do
                labeled "Move all charges to your resource pool" do
                  spendUses (attrs.ability 1) aid Charge n
                  gainResources iid (attrs.ability 1) n
                  toDiscardBy iid (attrs.ability 1) aid

      pure a
    _ -> SpiritSpeaker <$> liftRunMessage msg attrs
