module Arkham.Location.Cards.RailExit (railExit) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RailExit = RailExit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

railExit :: LocationCard RailExit
railExit = location RailExit Cards.railExit 1 (Static 0)

instance HasAbilities RailExit where
  getAbilities (RailExit a) =
    extendRevealed a []

instance RunMessage RailExit where
  runMessage msg (RailExit attrs) = runQueueT $ case msg of
    _ -> RailExit <$> liftRunMessage msg attrs
