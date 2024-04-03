module Arkham.Asset.Cards.TheNecronomicon (TheNecronomicon (..), theNecronomicon) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude
import Arkham.Token

newtype TheNecronomicon = TheNecronomicon AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNecronomicon :: AssetCard TheNecronomicon
theNecronomicon =
  assetWith TheNecronomicon Cards.theNecronomicon
    $ (tokensL %~ setTokens Horror 3)
    . (canLeavePlayByNormalMeansL .~ False)

instance HasModifiersFor TheNecronomicon where
  getModifiersFor (ChaosTokenTarget token) (TheNecronomicon a) = do
    case a.controller of
      Just iid | token.revealedBy == Just iid -> do
        pure $ toModifiers a [ForcedChaosTokenChange #eldersign [#autofail]]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities TheNecronomicon where
  getAbilities (TheNecronomicon a) = [controlledAbility a 1 AnyHorrorOnThis #action]

instance RunMessage TheNecronomicon where
  runMessage msg a@(TheNecronomicon attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushWhen (attrs.horror <= 1) $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      push $ MovedHorror (toAbilitySource attrs 1) (toTarget iid) 1
      pure a
    _ -> TheNecronomicon <$> runMessage msg attrs
