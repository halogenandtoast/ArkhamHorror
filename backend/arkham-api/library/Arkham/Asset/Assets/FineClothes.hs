module Arkham.Asset.Assets.FineClothes (fineClothes) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, isParley)

newtype FineClothes = FineClothes AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fineClothes :: AssetCard FineClothes
fineClothes = assetWith FineClothes Cards.fineClothes $ (healthL ?~ 1) . (sanityL ?~ 1)

instance HasModifiersFor FineClothes where
  getModifiersFor (FineClothes a) =
    getSkillTest >>= traverse_ \st -> do
      maybeModified_ a (SkillTestTarget st.id) do
        guard $ a `controlledBy` st.investigator
        liftGuardM isParley
        pure [Difficulty (-2)]

instance RunMessage FineClothes where
  runMessage msg (FineClothes attrs) = FineClothes <$> runMessage msg attrs
