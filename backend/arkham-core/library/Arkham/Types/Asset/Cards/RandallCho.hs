module Arkham.Types.Asset.Cards.RandallCho
  ( randallCho
  , RandallCho(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes

newtype RandallCho = RandallCho AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

randallCho :: AssetCard RandallCho
randallCho = ally RandallCho Cards.randallCho (1, 3)

instance AssetRunner env => RunMessage env RandallCho where
  runMessage msg (RandallCho attrs) = RandallCho <$> runMessage msg attrs
