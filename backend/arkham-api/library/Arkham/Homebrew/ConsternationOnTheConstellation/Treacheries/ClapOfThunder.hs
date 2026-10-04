module Arkham.Homebrew.ConsternationOnTheConstellation.Treacheries.ClapOfThunder (clapOfThunder) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ClapOfThunder = ClapOfThunder TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Test [intellect] (3). On a failure choose a skill icon and discard every card in
your hand showing at least one of it.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
clapOfThunder :: TreacheryCard ClapOfThunder
clapOfThunder = treachery ClapOfThunder Cards.clapOfThunder

instance RunMessage ClapOfThunder where
  runMessage msg (ClapOfThunder attrs) = runQueueT $ ClapOfThunder <$> liftRunMessage msg attrs
