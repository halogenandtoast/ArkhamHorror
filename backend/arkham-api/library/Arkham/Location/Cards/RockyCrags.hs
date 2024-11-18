module Arkham.Location.Cards.RockyCrags (rockyCrags, RockyCrags (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RockyCrags = RockyCrags LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rockyCrags :: LocationCard RockyCrags
rockyCrags = symbolLabel $ location RockyCrags Cards.rockyCrags 0 (Static 0)

instance HasAbilities RockyCrags where
  getAbilities (RockyCrags attrs) =
    extendRevealed attrs []

instance RunMessage RockyCrags where
  runMessage msg (RockyCrags attrs) = runQueueT $ case msg of
    _ -> RockyCrags <$> liftRunMessage msg attrs
