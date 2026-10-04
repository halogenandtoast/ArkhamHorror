module Arkham.Homebrew.ConsternationOnTheConstellation.Locations.Galley (galley) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype Galley = Galley LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Deck 2. Action, while Galley has no clues: a test frees the crew from the
refrigerators, sealing (-4) and adding a [cultist] token to the bag for the rest
of the scenario.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
galley :: LocationCard Galley
galley = location Galley Cards.galley 3 (PerPlayer 1)

instance RunMessage Galley where
  runMessage msg (Galley attrs) = runQueueT $ Galley <$> liftRunMessage msg attrs
