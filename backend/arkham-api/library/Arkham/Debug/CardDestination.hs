{-# LANGUAGE TemplateHaskell #-}

-- | Where the debug UI can send a card. See @DebugMoveCard@ in "Arkham.Message".
module Arkham.Debug.CardDestination where

import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.Deck
import Arkham.Id
import Arkham.Prelude
import Data.Aeson.TH

data DebugDeckPosition = DebugDeckTop | DebugDeckBottom | DebugDeckShuffle
  deriving stock (Show, Eq, Ord, Data)

data DebugCardDestination
  = DebugCardRemovedFromGame
  | DebugCardSetAside
  | DebugCardDiscard
  | DebugCardHand InvestigatorId
  | DebugCardDeck DeckSignifier DebugDeckPosition
  deriving stock (Show, Eq, Ord, Data)

{- | Whether a card may sit face down in the given deck.

A double-sided card has no deck back at all, and a deck only holds cards whose
back matches it -- @PutCardOnTopOfDeck _ EncounterDeck@ 'error's outright on a
player card, so this has to be checked before anything is pushed.
-}
cardDefCanEnterDeck :: CardDef -> DeckSignifier -> Bool
cardDefCanEnterDeck def = \case
  InvestigatorDeck _ -> isPlayer
  InvestigatorDiscard _ -> isPlayer
  InvestigatorDeckByKey _ _ -> isPlayer
  EncounterDeck -> isEncounter
  EncounterDiscard -> isEncounter
  EncounterDeckByKey _ -> isEncounter
  -- Scenario decks are bespoke piles that hold whatever the scenario put in
  -- them (the Summit deck is all locations), so only the two-sided rule applies.
  ScenarioDeckByKey _ -> not (cdDoubleSided def)
  NoDeck -> False
 where
  isPlayer = not (cdDoubleSided def) && cdCardType def `elem` playerCardTypes
  isEncounter = not (cdDoubleSided def) && cdCardType def `elem` encounterCardTypes

{- | Whether a deck implements \"put this on the top/bottom\".

Only these three deck signifiers have @PutCardOnTopOfDeck@ \/
@PutCardOnBottomOfDeck@ handlers; for every other deck those messages fall
through and do nothing, which for a card that has already been obtained means
losing it.
-}
deckSupportsEnds :: DeckSignifier -> Bool
deckSupportsEnds = \case
  InvestigatorDeck _ -> True
  EncounterDeck -> True
  ScenarioDeckByKey _ -> True
  _ -> False

$(deriveJSON defaultOptions ''DebugDeckPosition)
$(deriveJSON defaultOptions ''DebugCardDestination)
