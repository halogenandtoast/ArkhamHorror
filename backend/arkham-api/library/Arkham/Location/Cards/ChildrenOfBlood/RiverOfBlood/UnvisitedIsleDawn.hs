module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.UnvisitedIsleDawn (unvisitedIsleDawn) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype UnvisitedIsleDawn = UnvisitedIsleDawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleDawn :: LocationCard UnvisitedIsleDawn
unvisitedIsleDawn =
  location UnvisitedIsleDawn Cards.unvisitedIsleDawn 3 (PerPlayer 3)
    & setLabel "unvisitedIsle"

instance HasAbilities UnvisitedIsleDawn where
  getAbilities (UnvisitedIsleDawn a) =
    extendRevealed a []

instance RunMessage UnvisitedIsleDawn where
  runMessage msg (UnvisitedIsleDawn attrs) = runQueueT $ case msg of
    _ -> UnvisitedIsleDawn <$> liftRunMessage msg attrs
