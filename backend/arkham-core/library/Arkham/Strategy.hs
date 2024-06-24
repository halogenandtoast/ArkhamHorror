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
  | DamageDirect
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
  | RemoveRestFromGame
  deriving stock (Show, Eq, Ord, Data)

-- NOTE: INT must be the number of targets to resolve
data FoundCardsStrategy
  = PlayFound InvestigatorId Int
  | PlayFoundNoCost InvestigatorId Int
  | DrawFound InvestigatorId Int
  | DrawAllFound InvestigatorId
  | DrawFoundUpTo InvestigatorId Int
  | DeferSearchedToTarget Target IsDraw
  | ReturnCards
  | RemoveFoundFromGame InvestigatorId Int
  | DrawOrCommitFound InvestigatorId Int
  | AddToHandOrPlayFound InvestigatorId Int
  deriving stock (Show, Eq, Ord, Data)

data IsDraw = IsDraw | IsNotDraw
  deriving stock (Show, Eq, Ord, Data)

isSearchDraw :: FoundCardsStrategy -> Bool
isSearchDraw = \case
  PlayFound {} -> False
  PlayFoundNoCost {} -> False
  DrawFound {} -> True
  DrawAllFound {} -> True
  DrawFoundUpTo {} -> True
  DeferSearchedToTarget _ isDraw -> isDraw == IsDraw
  ReturnCards -> False
  RemoveFoundFromGame {} -> False
  DrawOrCommitFound {} -> True
  AddToHandOrPlayFound {} -> False

defer :: Targetable target => target -> IsDraw -> FoundCardsStrategy
defer t = DeferSearchedToTarget (toTarget t)

data AfterPlayStrategy
  = DiscardThis
  | RemoveThisFromGame
  | ShuffleThisBackIntoDeck
  | ReturnThisToHand
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

$(deriveJSON defaultOptions ''IsDraw)
$(deriveJSON defaultOptions ''DamageStrategy)
$(deriveJSON defaultOptions ''ZoneReturnStrategy)
$(deriveJSON defaultOptions ''FoundCardsStrategy)
$(deriveJSON defaultOptions ''AfterPlayStrategy)
$(deriveJSON defaultOptions ''ChosenCardStrategy)
