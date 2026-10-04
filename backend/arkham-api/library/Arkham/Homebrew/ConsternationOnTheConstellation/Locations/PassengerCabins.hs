module Arkham.Homebrew.ConsternationOnTheConstellation.Locations.PassengerCabins (passengerCabins) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype PassengerCabins = PassengerCabins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Deck 2. Forced when revealed: search the encounter deck and discard pile for a
Cultist enemy and spawn it here. Defeating it frees the passengers, sealing (-4)
here and adding a token to the bag for the rest of the scenario.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
passengerCabins :: LocationCard PassengerCabins
passengerCabins = location PassengerCabins Cards.passengerCabins 2 (Static 0)

instance RunMessage PassengerCabins where
  runMessage msg (PassengerCabins attrs) = runQueueT $ PassengerCabins <$> liftRunMessage msg attrs
