module Arkham.Homebrew.ConsternationOnTheConstellation.Locations.Lifeboat (lifeboat) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype Lifeboat = Lifeboat LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Set aside until act 3a ("Flee the Ship") lifts it into play. Forced when
revealed: discard encounter cards until a Deep One is discarded and spawn it
here. Objective: with no ready enemies here and everyone aboard, spend two clues
per investigator to reach resolution 1. Victory 2.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
lifeboat :: LocationCard Lifeboat
lifeboat = location Lifeboat Cards.lifeboat 4 (PerPlayer 3)

instance RunMessage Lifeboat where
  runMessage msg (Lifeboat attrs) = runQueueT $ Lifeboat <$> liftRunMessage msg attrs
