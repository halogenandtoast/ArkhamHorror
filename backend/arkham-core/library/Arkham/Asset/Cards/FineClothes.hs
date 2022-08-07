module Arkham.Asset.Cards.FineClothes
  ( fineClothes
  , FineClothes(..)
  ) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.SkillTest
import Arkham.Source
import Arkham.Target

newtype FineClothes = FineClothes AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fineClothes :: AssetCard FineClothes
fineClothes =
  assetWith FineClothes Cards.fineClothes $ (healthL ?~ 1) . (sanityL ?~ 1)

instance HasModifiersFor FineClothes where
  getModifiersFor SkillTestTarget (FineClothes a) = do
    mSkillTestSource <- getSkillTestSource
    case mSkillTestSource of
      Just (SkillTestSource iid _ _ (Just Action.Parley))
        | controlledBy a iid -> pure $ toModifiers a [Difficulty (-2)]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage FineClothes where
  runMessage msg (FineClothes attrs) = FineClothes <$> runMessage msg attrs
