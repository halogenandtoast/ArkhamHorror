module Arkham.Asset.Cards.FineClothes (
  fineClothes,
  FineClothes (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype FineClothes = FineClothes AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

fineClothes :: AssetCard FineClothes
fineClothes =
  assetWith FineClothes Cards.fineClothes $ (healthL ?~ 1) . (sanityL ?~ 1)

instance HasModifiersFor FineClothes where
  getModifiersFor SkillTestTarget (FineClothes a) = do
    mAction <- getSkillTestAction
    miid <- getSkillTestInvestigator
    case (mAction, miid) of
      (Just Action.Parley, Just iid) | controlledBy a iid -> do
        pure $ toModifiers a [Difficulty (-2)]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage FineClothes where
  runMessage msg (FineClothes attrs) = FineClothes <$> runMessage msg attrs
