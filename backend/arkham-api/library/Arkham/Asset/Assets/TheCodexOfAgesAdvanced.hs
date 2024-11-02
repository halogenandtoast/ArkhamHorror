module Arkham.Asset.Assets.TheCodexOfAgesAdvanced (
  theCodexOfAgesAdvanced,
  TheCodexOfAgesAdvanced (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Modifier

newtype TheCodexOfAgesAdvanced = TheCodexOfAgesAdvanced AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCodexOfAgesAdvanced :: AssetCard TheCodexOfAgesAdvanced
theCodexOfAgesAdvanced = asset TheCodexOfAgesAdvanced Cards.theCodexOfAgesAdvanced

instance HasModifiersFor TheCodexOfAgesAdvanced where
  getModifiersFor (InvestigatorTarget iid) (TheCodexOfAgesAdvanced a) | controlledBy a iid = do
    toModifiers a [SkillModifier sType 1 | notNull (assetSealedChaosTokens a), sType <- [minBound ..]]
  getModifiersFor _ _ = pure []

instance RunMessage TheCodexOfAgesAdvanced where
  runMessage msg (TheCodexOfAgesAdvanced attrs) = runQueueT $ case msg of
    _ -> TheCodexOfAgesAdvanced <$> liftRunMessage msg attrs
