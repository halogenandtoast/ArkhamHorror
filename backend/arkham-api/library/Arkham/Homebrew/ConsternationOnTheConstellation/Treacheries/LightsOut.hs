module Arkham.Homebrew.ConsternationOnTheConstellation.Treacheries.LightsOut (lightsOut) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LightsOut = LightsOut TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Attaches to your location, limit one per location. That location gets +2 shroud
and you cannot play assets costing less than its shroud while you are there.
Discarded once the location is successfully investigated.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
lightsOut :: TreacheryCard LightsOut
lightsOut = treachery LightsOut Cards.lightsOut

instance RunMessage LightsOut where
  runMessage msg (LightsOut attrs) = runQueueT $ LightsOut <$> liftRunMessage msg attrs
