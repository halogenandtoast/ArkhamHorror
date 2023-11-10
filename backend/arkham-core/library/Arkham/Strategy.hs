{-# LANGUAGE TemplateHaskell #-}

module Arkham.Strategy where

import Arkham.Card.CardDef
import Arkham.Id
import Arkham.Prelude
import Arkham.Target
import Arkham.Zone
import Data.Aeson.TH

data DamageStrategy
  = DamageAny
  | DamageAssetsFirst
  | DamageFirst CardDef
  | SingleTarget
  | DamageEvenly
  | -- Hastur has specific damage rules
    DamageFromHastur
  deriving stock (Show, Eq, Ord, Data)

data ZoneReturnStrategy
  = PutBackInAnyOrder
  | ShuffleBackIn
  | PutBack
  | DiscardRest
  deriving stock (Show, Eq, Ord, Data)

-- NOTE: INT must be the number of targets to resolve
data FoundCardsStrategy
  = PlayFound InvestigatorId Int
  | PlayFoundNoCost InvestigatorId Int
  | DrawFound InvestigatorId Int
  | DrawFoundUpTo InvestigatorId Int
  | DeferSearchedToTarget Target
  | ReturnCards
  | RemoveFoundFromGame InvestigatorId Int
  | DrawOrCommitFound InvestigatorId Int
  | DrawOrPlayFound InvestigatorId Int
  deriving stock (Show, Eq, Ord, Data)

defer :: Target -> FoundCardsStrategy
defer = DeferSearchedToTarget

data AfterPlayStrategy
  = DiscardThis
  | RemoveThisFromGame
  | ShuffleThisBackIntoDeck
  deriving stock (Show, Eq, Ord, Data)

data ChosenCardStrategy
  = LeaveChosenCard
  | RemoveChosenCardFromGame
  deriving stock (Show, Eq, Ord, Data)

fromTopOfDeck :: Int -> (Zone, ZoneReturnStrategy)
fromTopOfDeck n = (FromTopOfDeck n, ShuffleBackIn)

fromBottomOfDeck :: Int -> (Zone, ZoneReturnStrategy)
fromBottomOfDeck n = (FromBottomOfDeck n, ShuffleBackIn)

fromDeck :: (Zone, ZoneReturnStrategy)
fromDeck = (FromDeck, ShuffleBackIn)

fromDiscard :: (Zone, ZoneReturnStrategy)
fromDiscard = (FromDiscard, PutBack)

$(deriveJSON defaultOptions ''DamageStrategy)
$(deriveJSON defaultOptions ''ZoneReturnStrategy)
$(deriveJSON defaultOptions ''FoundCardsStrategy)
$(deriveJSON defaultOptions ''AfterPlayStrategy)
$(deriveJSON defaultOptions ''ChosenCardStrategy)
