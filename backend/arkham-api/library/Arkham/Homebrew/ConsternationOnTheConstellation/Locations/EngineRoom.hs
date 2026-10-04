module Arkham.Homebrew.ConsternationOnTheConstellation.Locations.EngineRoom (engineRoom) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype EngineRoom = EngineRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Deck 1. While Boiler Room's revealed side is clueless and Engine Room itself is
clueless, discarding one [book], one [combat] and one [agility] icon from hand
seals (-6), adds an [elder_thing] to the bag and records "restarted the engine".
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
engineRoom :: LocationCard EngineRoom
engineRoom = location EngineRoom Cards.engineRoom 4 (PerPlayer 1)

instance RunMessage EngineRoom where
  runMessage msg (EngineRoom attrs) = runQueueT $ EngineRoom <$> liftRunMessage msg attrs
