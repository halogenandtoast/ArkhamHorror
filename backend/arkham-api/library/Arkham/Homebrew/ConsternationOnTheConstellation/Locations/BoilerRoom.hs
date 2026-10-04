module Arkham.Homebrew.ConsternationOnTheConstellation.Locations.BoilerRoom (boilerRoom) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype BoilerRoom = BoilerRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Deck 1. Forced after you end your turn here: put the top card of your deck
facedown into your threat area as an Exhaustion treachery (-1 [combat], -1
[agility], -1 health) that discards when you leave.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
boilerRoom :: LocationCard BoilerRoom
boilerRoom = location BoilerRoom Cards.boilerRoom 3 (PerPlayer 1)

instance RunMessage BoilerRoom where
  runMessage msg (BoilerRoom attrs) = runQueueT $ BoilerRoom <$> liftRunMessage msg attrs
