module Arkham.Types.Asset.Cards.SebastienMoreau
  ( sebastienMoreau
  , SebastienMoreau(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes

newtype SebastienMoreau = SebastienMoreau AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sebastienMoreau :: AssetCard SebastienMoreau
sebastienMoreau = asset SebastienMoreau Cards.sebastienMoreau

instance AssetRunner env => RunMessage env SebastienMoreau where
  runMessage msg (SebastienMoreau attrs) =
    SebastienMoreau <$> runMessage msg attrs
