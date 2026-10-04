module Arkham.Homebrew.ConsternationOnTheConstellation.Acts.FleeTheShip (fleeTheShip) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Acts qualified as Cards

newtype FleeTheShip = FleeTheShip ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Act 3a, the branch reached by advancing act 2. Sun Deck gains a three-action
test to lift the Lifeboat into play. Objective: complete the objective printed on
Lifeboat. Its b side is a no-op that flips straight back to 3a.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
fleeTheShip :: ActCard FleeTheShip
fleeTheShip = act (3, A) FleeTheShip Cards.fleeTheShip Nothing

instance RunMessage FleeTheShip where
  runMessage msg (FleeTheShip attrs) = runQueueT $ FleeTheShip <$> liftRunMessage msg attrs
