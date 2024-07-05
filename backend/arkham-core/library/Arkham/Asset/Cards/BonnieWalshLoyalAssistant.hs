module Arkham.Asset.Cards.BonnieWalshLoyalAssistant (
  bonnieWalshLoyalAssistant,
  BonnieWalshLoyalAssistant (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetExhausted)
import Arkham.Matcher

newtype BonnieWalshLoyalAssistant = BonnieWalshLoyalAssistant AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bonnieWalshLoyalAssistant :: AssetCard BonnieWalshLoyalAssistant
bonnieWalshLoyalAssistant = ally BonnieWalshLoyalAssistant Cards.bonnieWalshLoyalAssistant (2, 2)

instance HasAbilities BonnieWalshLoyalAssistant where
  getAbilities (BonnieWalshLoyalAssistant x) =
    [ playerLimit PerRound
        $ restrictedAbility
          x
          1
          ( ControlsThis
              <> exists (AssetExhausted <> AssetControlledBy (HasMatchingAsset (be x)) <> not_ (be x))
          )
        $ freeReaction
        $ Exhausts #after You (TargetIs $ toTarget x)
    ]

instance RunMessage BonnieWalshLoyalAssistant where
  runMessage msg a@(BonnieWalshLoyalAssistant attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ AssetExhausted <> assetControlledBy iid <> not_ (be attrs)
      when (notNull assets) do
        chooseOne iid [targetLabel x [Ready (toTarget x)] | x <- assets]

      pure a
    _ -> BonnieWalshLoyalAssistant <$> liftRunMessage msg attrs
