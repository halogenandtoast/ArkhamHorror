module Arkham.Homebrew.ConsternationOnTheConstellation.Locations.DiningRoom (diningRoom) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype DiningRoom = DiningRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Deck 2. Action: spend 1 clue to evade an enemy at this or a connecting location.
Group limit once per game.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
diningRoom :: LocationCard DiningRoom
diningRoom = location DiningRoom Cards.diningRoom 2 (PerPlayer 1)

instance RunMessage DiningRoom where
  runMessage msg (DiningRoom attrs) = runQueueT $ DiningRoom <$> liftRunMessage msg attrs
