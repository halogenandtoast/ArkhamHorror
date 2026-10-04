module Arkham.Homebrew.ConsternationOnTheConstellation.Treacheries.SweptAway (sweptAway) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SweptAway = SweptAway TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Sinking Ship. Attaches to the current agenda, limit one per agenda. Entering an
exhausted location now costs an [agility] (3) test; failing it costs 3 resources
or a non-story Item asset. Discarded when the agenda would advance.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
sweptAway :: TreacheryCard SweptAway
sweptAway = treachery SweptAway Cards.sweptAway

instance RunMessage SweptAway where
  runMessage msg (SweptAway attrs) = runQueueT $ SweptAway <$> liftRunMessage msg attrs
