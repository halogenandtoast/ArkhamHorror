module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.UnvisitedIsleDusk (unvisitedIsleDusk) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype UnvisitedIsleDusk = UnvisitedIsleDusk LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleDusk :: LocationCard UnvisitedIsleDusk
unvisitedIsleDusk =
  location UnvisitedIsleDusk Cards.unvisitedIsleDusk 3 (PerPlayer 3)
    & setLabel "unvisitedIsle"

instance HasAbilities UnvisitedIsleDusk where
  getAbilities (UnvisitedIsleDusk a) =
    extendRevealed a []

instance RunMessage UnvisitedIsleDusk where
  runMessage msg (UnvisitedIsleDusk attrs) = runQueueT $ case msg of
    _ -> UnvisitedIsleDusk <$> liftRunMessage msg attrs
