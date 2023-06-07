module Arkham.Asset.Cards.AugustLindquist
  ( augustLindquist
  , AugustLindquist(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype AugustLindquist = AugustLindquist AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

augustLindquist :: AssetCard AugustLindquist
augustLindquist =
  asset AugustLindquist Cards.augustLindquist

instance RunMessage AugustLindquist where
  runMessage msg (AugustLindquist attrs) = AugustLindquist <$> runMessage msg attrs
