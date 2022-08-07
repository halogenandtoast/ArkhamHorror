module Arkham.Asset.Cards.LeoDeLuca1 where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Source
import Arkham.Target

newtype LeoDeLuca1 = LeoDeLuca1 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leoDeLuca1 :: AssetCard LeoDeLuca1
leoDeLuca1 = ally LeoDeLuca1 Cards.leoDeLuca1 (2, 2)

instance HasModifiersFor LeoDeLuca1 where
  getModifiersFor (InvestigatorTarget iid) (LeoDeLuca1 a) =
    pure [ toModifier a (AdditionalActions 1) | controlledBy a iid ]
  getModifiersFor _ _ = pure []

instance RunMessage LeoDeLuca1 where
  runMessage msg (LeoDeLuca1 attrs@AssetAttrs {..}) = case msg of
    InvestigatorPlayAsset iid aid | aid == assetId -> do
      push $ GainActions iid (AssetSource aid) 1
      LeoDeLuca1 <$> runMessage msg attrs
    _ -> LeoDeLuca1 <$> runMessage msg attrs
