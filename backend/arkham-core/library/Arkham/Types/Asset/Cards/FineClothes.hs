module Arkham.Types.Asset.Cards.FineClothes
  ( fineClothes
  , FineClothes(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target

newtype FineClothes = FineClothes AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fineClothes :: AssetCard FineClothes
fineClothes =
  assetWith FineClothes Cards.fineClothes $ (healthL ?~ 1) . (sanityL ?~ 1)

instance HasModifiersFor env FineClothes where
  getModifiersFor (SkillTestSource iid _ _ _ (Just Action.Parley)) (InvestigatorTarget iid') (FineClothes a)
    | ownedBy a iid && iid == iid'
    = pure $ toModifiers a [Difficulty (-2)]
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage env FineClothes where
  runMessage msg (FineClothes attrs) = FineClothes <$> runMessage msg attrs
