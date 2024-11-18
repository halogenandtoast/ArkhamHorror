module Arkham.Location.Cards.SnowGraves (snowGraves, SnowGraves (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SnowGraves = SnowGraves LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snowGraves :: LocationCard SnowGraves
snowGraves = symbolLabel $ location SnowGraves Cards.snowGraves 0 (Static 0)

instance HasAbilities SnowGraves where
  getAbilities (SnowGraves attrs) =
    extendRevealed attrs []

instance RunMessage SnowGraves where
  runMessage msg (SnowGraves attrs) = runQueueT $ case msg of
    _ -> SnowGraves <$> liftRunMessage msg attrs
