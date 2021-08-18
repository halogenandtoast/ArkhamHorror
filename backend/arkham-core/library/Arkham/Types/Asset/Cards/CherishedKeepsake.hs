module Arkham.Types.Asset.Cards.CherishedKeepsake where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes

newtype CherishedKeepsake = CherishedKeepsake AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

cherishedKeepsake :: AssetCard CherishedKeepsake
cherishedKeepsake =
  accessoryWith CherishedKeepsake Cards.cherishedKeepsake (sanityL ?~ 2)

instance HasAbilities env CherishedKeepsake where
  getAbilities i window (CherishedKeepsake x) = getAbilities i window x

instance (AssetRunner env) => RunMessage env CherishedKeepsake where
  runMessage msg (CherishedKeepsake attrs) =
    CherishedKeepsake <$> runMessage msg attrs
