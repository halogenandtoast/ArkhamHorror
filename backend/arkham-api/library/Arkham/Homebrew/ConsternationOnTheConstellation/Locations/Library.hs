module Arkham.Homebrew.ConsternationOnTheConstellation.Locations.Library (library) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype Library = Library LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Deck 2. Action: spend one clue per investigator to test [intellect] (3); on a
success seal (-5) and add a token to the bag. Forced at the end of the round:
top the Library back up to one clue per investigator. Victory 5.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
library :: LocationCard Library
library = location Library Cards.library 3 (PerPlayer 3)

instance RunMessage Library where
  runMessage msg (Library attrs) = runQueueT $ Library <$> liftRunMessage msg attrs
