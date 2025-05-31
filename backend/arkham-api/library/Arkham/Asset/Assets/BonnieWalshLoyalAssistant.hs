module Arkham.Asset.Assets.BonnieWalshLoyalAssistant (bonnieWalshLoyalAssistant) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetExhausted)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype BonnieWalshLoyalAssistant = BonnieWalshLoyalAssistant AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bonnieWalshLoyalAssistant :: AssetCard BonnieWalshLoyalAssistant
bonnieWalshLoyalAssistant = ally BonnieWalshLoyalAssistant Cards.bonnieWalshLoyalAssistant (2, 2)

instance HasAbilities BonnieWalshLoyalAssistant where
  getAbilities (BonnieWalshLoyalAssistant x) =
    [ playerLimit PerRound
        $ controlled
          x
          1
          (exists (#exhausted <> #ally <> AssetControlledBy (HasMatchingAsset (be x)) <> not_ (be x)))
        $ freeReaction
        $ Exhausts #after You (TargetIs $ toTarget x)
    ]

instance RunMessage BonnieWalshLoyalAssistant where
  runMessage msg a@(BonnieWalshLoyalAssistant attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ #ally <> AssetExhausted <> assetControlledBy iid <> not_ (be attrs)
      chooseTargetM iid assets readyThis
      pure a
    _ -> BonnieWalshLoyalAssistant <$> liftRunMessage msg attrs
