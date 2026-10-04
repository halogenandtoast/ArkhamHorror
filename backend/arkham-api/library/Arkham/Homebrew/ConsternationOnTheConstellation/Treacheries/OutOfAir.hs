module Arkham.Homebrew.ConsternationOnTheConstellation.Treacheries.OutOfAir (outOfAir) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OutOfAir = OutOfAir TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Sinking Ship. Away from an exhausted location it simply surges. At one, a failed
[agility] (3) test deals 1 direct damage and puts this into your threat area,
where it deals 2 damage at the end of each of your turns until you leave.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
outOfAir :: TreacheryCard OutOfAir
outOfAir = treachery OutOfAir Cards.outOfAir

instance RunMessage OutOfAir where
  runMessage msg (OutOfAir attrs) = runQueueT $ OutOfAir <$> liftRunMessage msg attrs
