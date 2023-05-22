module Arkham.Asset.Cards.LeoDeLuca where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype LeoDeLuca = LeoDeLuca AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leoDeLuca :: AssetCard LeoDeLuca
leoDeLuca = ally LeoDeLuca Cards.leoDeLuca (2, 2)

instance HasModifiersFor LeoDeLuca where
  getModifiersFor (InvestigatorTarget iid) (LeoDeLuca a) =
    pure [ toModifier a (AdditionalActions 1) | controlledBy a iid ]
  getModifiersFor _ _ = pure []

instance RunMessage LeoDeLuca where
  runMessage msg (LeoDeLuca attrs@AssetAttrs {..}) = case msg of
    InvestigatorPlayAsset iid aid | aid == assetId -> do
      push $ GainActions iid (AssetSource aid) 1
      LeoDeLuca <$> runMessage msg attrs
    _ -> LeoDeLuca <$> runMessage msg attrs
