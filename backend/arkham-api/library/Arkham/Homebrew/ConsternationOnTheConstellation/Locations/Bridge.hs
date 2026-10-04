module Arkham.Homebrew.ConsternationOnTheConstellation.Locations.Bridge (bridge) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype Bridge = Bridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Deck 3. Action, if you have "restarted the engine": test [intellect] (3) to add
an [elder_sign] to the chaos bag for the rest of the scenario. Group limit once
per game. Victory 1.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
bridge :: LocationCard Bridge
bridge = location Bridge Cards.bridge 4 (PerPlayer 1)

instance RunMessage Bridge where
  runMessage msg (Bridge attrs) = runQueueT $ Bridge <$> liftRunMessage msg attrs
