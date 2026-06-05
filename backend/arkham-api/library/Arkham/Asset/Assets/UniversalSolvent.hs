module Arkham.Asset.Assets.UniversalSolvent (universalSolvent) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype UniversalSolvent = UniversalSolvent AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

universalSolvent :: AssetCard UniversalSolvent
universalSolvent = asset UniversalSolvent Cards.universalSolvent

instance RunMessage UniversalSolvent where
  runMessage msg (UniversalSolvent attrs) = UniversalSolvent <$> runMessage msg attrs
