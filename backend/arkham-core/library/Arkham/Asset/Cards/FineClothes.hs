module Arkham.Asset.Cards.FineClothes (fineClothes, FineClothes (..)) where

import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype FineClothes = FineClothes AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fineClothes :: AssetCard FineClothes
fineClothes = assetWith FineClothes Cards.fineClothes $ (healthL ?~ 1) . (sanityL ?~ 1)

instance HasModifiersFor FineClothes where
  getModifiersFor (SkillTestTarget _) (FineClothes a) = maybeModified a do
    Action.Parley <- MaybeT getSkillTestAction
    iid <- MaybeT getSkillTestInvestigator
    guard $ a `controlledBy` iid
    pure [Difficulty (-2)]
  getModifiersFor _ _ = pure []

instance RunMessage FineClothes where
  runMessage msg (FineClothes attrs) = FineClothes <$> runMessage msg attrs
