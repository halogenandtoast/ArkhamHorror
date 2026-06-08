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
    andM [emptyDeckCanBeShuffledInto deck, maybe (pure True) getCanShuffleDeckX deck.investigator]

emptyDeckCanBeShuffledInto :: (HasGame m, Tracing m) => Deck.DeckSignifier -> m Bool
emptyDeckCanBeShuffledInto Deck.NoDeck = pure False
emptyDeckCanBeShuffledInto deck
  | preventsShuffleIntoEmptyDeck deck = not <$> isDeckEmpty deck
  | otherwise = pure True

preventsShuffleIntoEmptyDeck :: Deck.DeckSignifier -> Bool
preventsShuffleIntoEmptyDeck = \case
  Deck.InvestigatorDeck _ -> True
  Deck.EncounterDeck -> True
  Deck.EncounterDeckByKey _ -> True
  _ -> False

instance CanShuffleIn Card
instance CanShuffleIn a => CanShuffleIn (Only a)
instance CanShuffleIn a => CanShuffleIn (NonEmpty a)
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
