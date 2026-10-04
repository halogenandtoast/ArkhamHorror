module Arkham.Homebrew.ConsternationOnTheConstellation.Locations.SunDeck (sunDeck) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype SunDeck = SunDeck LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Deck 3. Fight action: test against an enemy's fight value to shuffle it back
into the encounter deck; on a failure you are thrown into Open Water. Act 3a
("Flee the Ship") also grants the ability that launches the Lifeboat.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
sunDeck :: LocationCard SunDeck
sunDeck = location SunDeck Cards.sunDeck 2 (Static 0)

instance RunMessage SunDeck where
  runMessage msg (SunDeck attrs) = runQueueT $ SunDeck <$> liftRunMessage msg attrs
