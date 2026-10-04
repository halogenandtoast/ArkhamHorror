module Arkham.Homebrew.ConsternationOnTheConstellation.Locations.CargoRoom (cargoRoom) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype CargoRoom = CargoRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Deck 1, and where everyone starts. Action: test [intellect] (0) and search the
top X cards of your deck for an Item asset, where X is the amount you succeeded
by. Limit once per turn.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
cargoRoom :: LocationCard CargoRoom
cargoRoom = location CargoRoom Cards.cargoRoom 3 (PerPlayer 1)

instance RunMessage CargoRoom where
  runMessage msg (CargoRoom attrs) = runQueueT $ CargoRoom <$> liftRunMessage msg attrs
