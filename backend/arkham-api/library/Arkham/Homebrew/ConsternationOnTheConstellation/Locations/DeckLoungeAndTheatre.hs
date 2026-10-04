module Arkham.Homebrew.ConsternationOnTheConstellation.Locations.DeckLoungeAndTheatre (deckLoungeAndTheatre) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype DeckLoungeAndTheatre = DeckLoungeAndTheatre LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Deck 3. Forced after you enter: lose 1 resource for each action you have left.
Action: spend 1 clue to put the top card of your deck into play facedown as a
Disguise asset that can make every enemy aloof for the round.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
deckLoungeAndTheatre :: LocationCard DeckLoungeAndTheatre
deckLoungeAndTheatre = location DeckLoungeAndTheatre Cards.deckLoungeAndTheatre 4 (PerPlayer 1)

instance RunMessage DeckLoungeAndTheatre where
  runMessage msg (DeckLoungeAndTheatre attrs) = runQueueT $ DeckLoungeAndTheatre <$> liftRunMessage msg attrs
