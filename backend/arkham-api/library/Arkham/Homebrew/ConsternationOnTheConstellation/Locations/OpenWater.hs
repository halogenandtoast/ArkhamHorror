module Arkham.Homebrew.ConsternationOnTheConstellation.Locations.OpenWater (openWater) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype OpenWater = OpenWater LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | The ocean itself, off the ship, so it carries no Deck trait and never floods.
Leaving it is a test you may pay extra actions to lower, and ending your turn
here costs 2 damage or a non-story Item asset.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
openWater :: LocationCard OpenWater
openWater = location OpenWater Cards.openWater 6 (Static 0)

instance RunMessage OpenWater where
  runMessage msg (OpenWater attrs) = runQueueT $ OpenWater <$> liftRunMessage msg attrs
