module Arkham.Homebrew.ConsternationOnTheConstellation.Treacheries.SealedDoors (sealedDoors) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SealedDoors = SealedDoors TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Attaches to your location. While attached you cannot enter or leave that location
except by scenario card effects. An action and a [combat] (4) test discards it.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
sealedDoors :: TreacheryCard SealedDoors
sealedDoors = treachery SealedDoors Cards.sealedDoors

instance RunMessage SealedDoors where
  runMessage msg (SealedDoors attrs) = runQueueT $ SealedDoors <$> liftRunMessage msg attrs
