module Arkham.Types.Asset.Cards.CherishedKeepsake where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Asset.Attrs

newtype CherishedKeepsake = CherishedKeepsake AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

cherishedKeepsake :: AssetCard CherishedKeepsake
cherishedKeepsake =
  accessoryWith CherishedKeepsake Cards.cherishedKeepsake (sanityL ?~ 2)

instance AssetRunner env => RunMessage env CherishedKeepsake where
  runMessage msg (CherishedKeepsake attrs) =
    CherishedKeepsake <$> runMessage msg attrs
