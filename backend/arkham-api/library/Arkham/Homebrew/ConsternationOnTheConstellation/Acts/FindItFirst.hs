module Arkham.Homebrew.ConsternationOnTheConstellation.Acts.FindItFirst (findItFirst) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Acts qualified as Cards

newtype FindItFirst = FindItFirst ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Act 2a. Objective: advance when an investigator takes control of the Tablet of
Dagon. Advancing shuffles the Deep Ones and Sinking Ship sets into the encounter
deck, exhausts every Deck 1 location, spawns the Colossal Servant at Open Water,
and sets up the "Flee the Ship" branch.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
findItFirst :: ActCard FindItFirst
findItFirst = act (2, A) FindItFirst Cards.findItFirst Nothing

instance RunMessage FindItFirst where
  runMessage msg (FindItFirst attrs) = runQueueT $ FindItFirst <$> liftRunMessage msg attrs
