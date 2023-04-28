{-# LANGUAGE TemplateHaskell #-}
module Arkham.Strategy where

import Arkham.Prelude
import Arkham.Card.CardDef
import Arkham.Id
import Arkham.Target
import Arkham.Zone
import Data.Aeson.TH

data DamageStrategy
  = DamageAny
  | DamageAssetsFirst
  | DamageFirst CardDef
  | SingleTarget
  | DamageEvenly
  -- Hastur has specific damage rules
  | DamageFromHastur
  deriving stock (Show, Eq, Ord)

data ZoneReturnStrategy
  = PutBackInAnyOrder
  | ShuffleBackIn
  | PutBack
  | DiscardRest
  deriving stock (Show, Eq, Ord)

data FoundCardsStrategy
  = PlayFound InvestigatorId Int
  | PlayFoundNoCost InvestigatorId Int
  | DrawFound InvestigatorId Int
  | DrawFoundUpTo InvestigatorId Int
  | DeferSearchedToTarget Target
  | ReturnCards
  deriving stock (Show, Eq, Ord)

data AfterPlayStrategy
  = DiscardThis
  | RemoveThisFromGame
  deriving stock (Show, Eq, Ord)

data ChosenCardStrategy
  = LeaveChosenCard
  | RemoveChosenCardFromGame
  deriving stock (Show, Eq, Ord)

fromTopOfDeck :: Int -> (Zone, ZoneReturnStrategy)
fromTopOfDeck n = (FromTopOfDeck n, ShuffleBackIn)

fromBottomOfDeck :: Int -> (Zone, ZoneReturnStrategy)
fromBottomOfDeck n = (FromBottomOfDeck n, ShuffleBackIn)

fromDeck :: (Zone, ZoneReturnStrategy)
fromDeck = (FromDeck, ShuffleBackIn)

$(deriveJSON defaultOptions ''DamageStrategy)
$(deriveJSON defaultOptions ''ZoneReturnStrategy)
$(deriveJSON defaultOptions ''FoundCardsStrategy)
$(deriveJSON defaultOptions ''AfterPlayStrategy)
$(deriveJSON defaultOptions ''ChosenCardStrategy)
