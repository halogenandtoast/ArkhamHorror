module Arkham.Types.Asset.Cards.JordanPerry
  ( jordanPerry
  , JordanPerry(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes

newtype JordanPerry = JordanPerry AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jordanPerry :: AssetCard JordanPerry
jordanPerry = asset JordanPerry Cards.jordanPerry

instance AssetRunner env => RunMessage env JordanPerry where
  runMessage msg (JordanPerry attrs) = JordanPerry <$> runMessage msg attrs
