module Arkham.Location.Cards.SnowCoveredCrag (snowCoveredCrag) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SnowCoveredCrag = SnowCoveredCrag LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snowCoveredCrag :: LocationCard SnowCoveredCrag
snowCoveredCrag =
  locationWith
    SnowCoveredCrag
    Cards.snowCoveredCrag
    2
    (PerPlayer 2)
    (connectsToL .~ adjacentLocations)

instance HasAbilities SnowCoveredCrag where
  getAbilities (SnowCoveredCrag attrs) =
    extendRevealed attrs []

instance RunMessage SnowCoveredCrag where
  runMessage msg (SnowCoveredCrag attrs) = runQueueT $ case msg of
    _ -> SnowCoveredCrag <$> liftRunMessage msg attrs
