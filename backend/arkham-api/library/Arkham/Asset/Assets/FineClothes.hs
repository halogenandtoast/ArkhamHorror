module Arkham.Asset.Assets.FineClothes (fineClothes, FineClothes (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype FineClothes = FineClothes AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fineClothes :: AssetCard FineClothes
fineClothes = assetWith FineClothes Cards.fineClothes $ (healthL ?~ 1) . (sanityL ?~ 1)

instance HasModifiersFor FineClothes where
  getModifiersFor (FineClothes a) =
    getSkillTest >>= \case
      Nothing -> pure mempty
      Just st -> maybeModified_ a (SkillTestTarget st.id) do
        guard $ a `controlledBy` st.investigator
        liftGuardM isParley
        pure [Difficulty (-2)]

instance RunMessage FineClothes where
  runMessage msg (FineClothes attrs) = FineClothes <$> runMessage msg attrs
