module Arkham.Homebrew.ConsternationOnTheConstellation.Treacheries.CallOfRlyeh (callOfRlyeh) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CallOfRlyeh = CallOfRlyeh TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Deep Ones. Goes into your threat area. While a Deep One is in play, moving means
moving toward the nearest one or taking 1 horror; engaging a Deep One discards
it.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
callOfRlyeh :: TreacheryCard CallOfRlyeh
callOfRlyeh = treachery CallOfRlyeh Cards.callOfRlyeh

instance RunMessage CallOfRlyeh where
  runMessage msg (CallOfRlyeh attrs) = runQueueT $ CallOfRlyeh <$> liftRunMessage msg attrs
