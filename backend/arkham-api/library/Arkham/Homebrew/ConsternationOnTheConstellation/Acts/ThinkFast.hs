module Arkham.Homebrew.ConsternationOnTheConstellation.Acts.ThinkFast (thinkFast) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Acts qualified as Cards

newtype ThinkFast = ThinkFast ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Act 1a. Order Enforcer gains a Parley that lets investigators load him with
clues until he holds one per investigator and is discarded. Objective: advance
when there are no enemies in play. Advancing puts the set-aside locations into
play (except Lifeboat), attaches a random Crate of Goods to three of them, and
advances the agenda deck to 2a with all doom removed.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
thinkFast :: ActCard ThinkFast
thinkFast = act (1, A) ThinkFast Cards.thinkFast Nothing

instance RunMessage ThinkFast where
  runMessage msg (ThinkFast attrs) = runQueueT $ ThinkFast <$> liftRunMessage msg attrs
