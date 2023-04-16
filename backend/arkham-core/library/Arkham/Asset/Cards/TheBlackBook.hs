module Arkham.Asset.Cards.TheBlackBook
  ( theBlackBook
  , TheBlackBook(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype TheBlackBook = TheBlackBook AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackBook :: AssetCard TheBlackBook
theBlackBook = asset TheBlackBook Cards.theBlackBook

instance RunMessage TheBlackBook where
  runMessage msg (TheBlackBook attrs) = TheBlackBook <$> runMessage msg attrs
