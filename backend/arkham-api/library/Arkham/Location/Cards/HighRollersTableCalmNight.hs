module Arkham.Location.Cards.HighRollersTableCalmNight (highRollersTableCalmNight) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HighRollersTableCalmNight = HighRollersTableCalmNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

highRollersTableCalmNight :: LocationCard HighRollersTableCalmNight
highRollersTableCalmNight = symbolLabel $ location HighRollersTableCalmNight Cards.highRollersTableCalmNight 0 (Static 0)

instance HasAbilities HighRollersTableCalmNight where
  getAbilities (HighRollersTableCalmNight attrs) =
    extendRevealed attrs []

instance RunMessage HighRollersTableCalmNight where
  runMessage msg (HighRollersTableCalmNight attrs) = runQueueT $ case msg of
    _ -> HighRollersTableCalmNight <$> liftRunMessage msg attrs
