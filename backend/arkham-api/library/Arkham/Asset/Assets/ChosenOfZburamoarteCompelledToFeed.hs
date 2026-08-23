module Arkham.Asset.Assets.ChosenOfZburamoarteCompelledToFeed (chosenOfZburamoarteCompelledToFeed) where

import Arkham.Asset.Cards.ChildrenOfBlood qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ChosenOfZburamoarteCompelledToFeed = ChosenOfZburamoarteCompelledToFeed AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chosenOfZburamoarteCompelledToFeed :: AssetCard ChosenOfZburamoarteCompelledToFeed
chosenOfZburamoarteCompelledToFeed = asset ChosenOfZburamoarteCompelledToFeed Cards.chosenOfZburamoarteCompelledToFeed

instance RunMessage ChosenOfZburamoarteCompelledToFeed where
  runMessage msg (ChosenOfZburamoarteCompelledToFeed attrs) = ChosenOfZburamoarteCompelledToFeed <$> runMessage msg attrs
