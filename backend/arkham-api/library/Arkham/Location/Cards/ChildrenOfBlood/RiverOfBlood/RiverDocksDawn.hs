module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.RiverDocksDawn (riverDocksDawn) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype RiverDocksDawn = RiverDocksDawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverDocksDawn :: LocationCard RiverDocksDawn
riverDocksDawn = symbolLabel $ location RiverDocksDawn Cards.riverDocksDawn 2 (PerPlayer 1)

instance HasAbilities RiverDocksDawn where
  getAbilities (RiverDocksDawn a) =
    extendRevealed a []

instance RunMessage RiverDocksDawn where
  runMessage msg (RiverDocksDawn attrs) = runQueueT $ case msg of
    _ -> RiverDocksDawn <$> liftRunMessage msg attrs
