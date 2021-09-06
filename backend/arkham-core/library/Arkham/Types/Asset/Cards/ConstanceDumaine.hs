module Arkham.Types.Asset.Cards.ConstanceDumaine
  ( constanceDumaine
  , ConstanceDumaine(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes

newtype ConstanceDumaine = ConstanceDumaine AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

constanceDumaine :: AssetCard ConstanceDumaine
constanceDumaine = asset ConstanceDumaine Cards.constanceDumaine

instance AssetRunner env => RunMessage env ConstanceDumaine where
  runMessage msg (ConstanceDumaine attrs) =
    ConstanceDumaine <$> runMessage msg attrs
