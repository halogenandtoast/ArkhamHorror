{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -O0 #-}

module Arkham.Helpers.Window.Card where

import Arkham.Card
import Arkham.Deck
import Arkham.Id
import Arkham.Prelude
import Arkham.Window
import Arkham.Window qualified as Window

attachedCard :: HasCallStack => [Window] -> Card
attachedCard =
  fromMaybe (error "missing Attach card window") . asum . map \case
    (windowType -> Window.AttachCard _ card _) -> Just card
    _ -> Nothing

cardPlayed :: HasCallStack => [Window] -> Card
cardPlayed [] = error "missing play card window"
cardPlayed ((windowType -> Window.PlayCard _ c) : _) = c.card
cardPlayed (_ : xs) = cardPlayed xs

data DrawnCard = DrawnCard
  { card :: Card
  , drawnBy :: InvestigatorId
  , drawnFrom :: DeckSignifier
  }

type instance Element DrawnCard = Card

instance MonoFoldable DrawnCard where
  ofoldr f b x = f x.card b
  ofoldl' f a x = f a x.card
  otoList x = [x.card]
  ofoldMap f x = f x.card
  ofoldr1Ex f x = f x.card x.card
  ofoldl1Ex' f x = f x.card x.card

drawnCard :: HasCallStack => [Window] -> DrawnCard
drawnCard [] = error "missing play card window"
drawnCard ((windowType -> Window.DrawCard iid c deck) : _) = DrawnCard c iid deck
drawnCard (_ : xs) = drawnCard xs

cardDrawn :: HasCallStack => [Window] -> Card
cardDrawn [] = error "missing play card window"
cardDrawn ((windowType -> Window.DrawCard _ c _) : _) = c
cardDrawn (_ : xs) = cardDrawn xs

cancelledCard :: HasCallStack => [Window] -> CardId
cancelledCard [] = error "missing play card window"
cancelledCard ((windowType -> Window.CancelledOrIgnoredCardOrGameEffect _ (Just c)) : _) = c
cancelledCard (_ : xs) = cancelledCard xs

getPlayedEvent :: [Window] -> EventId
getPlayedEvent = \case
  [] -> error "getPlayedEvent: impossible"
  ((windowType -> Window.PlayEventDiscarding _ eventId) : _) -> eventId
  ((windowType -> Window.PlayEvent _ eventId) : _) -> eventId
  (_ : rest) -> getPlayedEvent rest

cardDiscarded :: HasCallStack => [Window] -> Card
cardDiscarded [] = error "missing play card window"
cardDiscarded ((windowType -> Window.DiscardedFromHand _ _ c) : _) = c
cardDiscarded ((windowType -> Window.Discarded _ _ c) : _) = c
cardDiscarded (_ : xs) = cardDiscarded xs

cardsDiscarded :: HasCallStack => [Window] -> [Card]
cardsDiscarded [] = []
cardsDiscarded ((windowType -> Window.DiscardedFromHand _ _ c) : ws) = c : cardsDiscarded ws
cardsDiscarded ((windowType -> Window.Discarded _ _ c) : ws) = c : cardsDiscarded ws
cardsDiscarded (_ : xs) = cardsDiscarded xs

cardDrawnBy :: HasCallStack => [Window] -> (InvestigatorId, Card)
cardDrawnBy [] = error "missing play card window"
cardDrawnBy ((windowType -> Window.DrawCard iid c _) : _) = (iid, c)
cardDrawnBy (_ : xs) = cardDrawnBy xs

cardsDrawn :: [Window] -> [Card]
cardsDrawn [] = []
cardsDrawn ((windowType -> Window.DrawCards _ cs) : rest) = cs <> cardsDrawn rest
cardsDrawn (_ : xs) = cardsDrawn xs

getCommittedCard :: [Window] -> Card
getCommittedCard [] = error "missing card"
getCommittedCard ((windowType -> Window.CommittedCard _ c) : _) = c
getCommittedCard (_ : ws) = getCommittedCard ws
