module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.RiverDocksDusk (riverDocksDusk) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype RiverDocksDusk = RiverDocksDusk LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverDocksDusk :: LocationCard RiverDocksDusk
riverDocksDusk = symbolLabel $ location RiverDocksDusk Cards.riverDocksDusk 3 (PerPlayer 1)

instance HasAbilities RiverDocksDusk where
  getAbilities (RiverDocksDusk a) =
    extendRevealed a []

instance RunMessage RiverDocksDusk where
  runMessage msg (RiverDocksDusk attrs) = runQueueT $ case msg of
    _ -> RiverDocksDusk <$> liftRunMessage msg attrs
