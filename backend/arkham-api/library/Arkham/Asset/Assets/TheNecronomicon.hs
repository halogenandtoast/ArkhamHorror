module Arkham.Asset.Assets.TheNecronomicon (theNecronomicon) where

import Arkham.Ability hiding (you)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (moveTokens)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Script
import GHC.Records

newtype TheNecronomicon = TheNecronomicon AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Sourceable)

instance HasField "horror" TheNecronomicon Int where
  getField = getField @"horror" . toAttrs

theNecronomicon :: AssetCard TheNecronomicon
theNecronomicon =
  assetWith TheNecronomicon Cards.theNecronomicon
    $ (tokensL %~ setTokens #horror 3)
    . (canLeavePlayByNormalMeansL .~ False)

instance HasModifiersFor TheNecronomicon where
  getModifiersFor (TheNecronomicon a) = for_ a.controller \iid -> do
    modifySelect a (ChaosTokenRevealedBy $ be iid) [ForcedChaosTokenChange #eldersign [#autofail]]

instance HasAbilities TheNecronomicon where
  getAbilities (TheNecronomicon a) = [controlledAbility a 1 AnyHorrorOnThis #action]

instance RunMessage TheNecronomicon where
  runMessage = script do
    revelation placeInYourThreatArea
    onAbilityThen
      1
      (moveTokens this you #horror 1)
      (when (this.horror == 0) discardThis)
