module Arkham.Asset.Cards.TheNecronomicon (
  TheNecronomicon (..),
  theNecronomicon,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.ChaosToken qualified as ChaosToken
import Arkham.Token
import Arkham.Window (defaultWindows)

newtype TheNecronomicon = TheNecronomicon AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNecronomicon :: AssetCard TheNecronomicon
theNecronomicon =
  assetWith TheNecronomicon Cards.theNecronomicon $
    (tokensL %~ setTokens Horror 3)
      . (canLeavePlayByNormalMeansL .~ False)

instance HasModifiersFor TheNecronomicon where
  getModifiersFor (ChaosTokenTarget (ChaosToken.chaosTokenFace -> ChaosToken.ElderSign)) (TheNecronomicon a) = do
    mInvestigator <- getSkillTestInvestigator
    case mInvestigator of
      Just iid ->
        pure
          [ toModifier a $ ForcedChaosTokenChange ChaosToken.ElderSign [ChaosToken.AutoFail]
          | controlledBy a iid
          ]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities TheNecronomicon where
  getAbilities (TheNecronomicon a) =
    [ restrictedAbility a 1 (ControlsThis <> AnyHorrorOnThis) $
        ActionAbility Nothing $
          ActionCost 1
    ]

instance RunMessage TheNecronomicon where
  runMessage msg a@(TheNecronomicon attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ PutCardIntoPlay iid (toCard attrs) Nothing (defaultWindows iid)
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      when (assetHorror attrs <= 1) $ do
        push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      push $ MovedHorror (toSource attrs) (InvestigatorTarget iid) 1
      pure a
    _ -> TheNecronomicon <$> runMessage msg attrs
