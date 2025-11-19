module Arkham.Helpers.Shuffle where

import Arkham.Capability
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Deck
import Arkham.Helpers.Window (DrawnCard)
import Arkham.Id
import Arkham.Prelude
import Arkham.Target
import Arkham.Tracing

class CanShuffleIn a where
  getCanShuffleIn :: (HasGame m, Tracing m, Deck.IsDeck deck) => deck -> a -> m Bool
  getCanShuffleIn (Deck.toDeck -> deck) _a =
    andM [not <$> isDeckEmpty deck, maybe (pure True) getCanShuffleDeckX deck.investigator]

instance CanShuffleIn Card
instance CanShuffleIn a => CanShuffleIn (Only a)
instance CanShuffleIn EncounterCard
instance CanShuffleIn PlayerCard
instance CanShuffleIn DrawnCard
instance CanShuffleIn Target
instance CanShuffleIn a => CanShuffleIn [a] where
  getCanShuffleIn _deck [] = pure False
  getCanShuffleIn deck [a] = getCanShuffleIn deck a
  getCanShuffleIn (Deck.toDeck -> deck) _ = maybe (pure True) can.shuffle.deck deck.investigator

getCanShuffleDeckX :: (HasGame m, Tracing m) => InvestigatorId -> m Bool
getCanShuffleDeckX = can.shuffle.deck

whenCanShuffleIn
  :: (HasGame m, Tracing m, CanShuffleIn c, Deck.IsDeck deck) => deck -> c -> m () -> m ()
whenCanShuffleIn deck c = whenM (getCanShuffleIn deck c)
