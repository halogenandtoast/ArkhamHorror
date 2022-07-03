module Arkham.Asset.Cards.JakeWilliams
  ( jakeWilliams
  , JakeWilliams(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype JakeWilliams = JakeWilliams AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jakeWilliams :: AssetCard JakeWilliams
jakeWilliams = ally JakeWilliams Cards.jakeWilliams (3, 2)

instance RunMessage JakeWilliams where
  runMessage msg (JakeWilliams attrs) = JakeWilliams <$> runMessage msg attrs
