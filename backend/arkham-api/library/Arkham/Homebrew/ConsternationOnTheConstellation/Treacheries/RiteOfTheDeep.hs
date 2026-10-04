module Arkham.Homebrew.ConsternationOnTheConstellation.Treacheries.RiteOfTheDeep (riteOfTheDeep) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RiteOfTheDeep = RiteOfTheDeep TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Place 1 doom on the nearest Cultist enemy; if you share a location with an enemy
that has doom, take 2 horror. With no Cultist enemies in play, search the
encounter deck and discard pile for one and draw it.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
riteOfTheDeep :: TreacheryCard RiteOfTheDeep
riteOfTheDeep = treachery RiteOfTheDeep Cards.riteOfTheDeep

instance RunMessage RiteOfTheDeep where
  runMessage msg (RiteOfTheDeep attrs) = runQueueT $ RiteOfTheDeep <$> liftRunMessage msg attrs
