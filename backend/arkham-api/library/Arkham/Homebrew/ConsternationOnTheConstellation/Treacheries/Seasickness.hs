module Arkham.Homebrew.ConsternationOnTheConstellation.Treacheries.Seasickness (seasickness) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Seasickness = Seasickness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Goes into your threat area. After you move, your next skill test this round gets
+1 difficulty; at the end of your turn an [intellect] (3) test discards it.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
seasickness :: TreacheryCard Seasickness
seasickness = treachery Seasickness Cards.seasickness

instance RunMessage Seasickness where
  runMessage msg (Seasickness attrs) = runQueueT $ Seasickness <$> liftRunMessage msg attrs
