module Arkham.Homebrew.ConsternationOnTheConstellation.Treacheries.TakingOnWater (takingOnWater) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TakingOnWater = TakingOnWater TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Sinking Ship. Peril. Exhaust a ready location with the lowest possible Deck
number -- the scenario's clock. Setup removes one copy per player beyond the
first.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
takingOnWater :: TreacheryCard TakingOnWater
takingOnWater = treachery TakingOnWater Cards.takingOnWater

instance RunMessage TakingOnWater where
  runMessage msg (TakingOnWater attrs) = runQueueT $ TakingOnWater <$> liftRunMessage msg attrs
