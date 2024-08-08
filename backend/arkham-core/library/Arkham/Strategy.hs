{-# LANGUAGE TemplateHaskell #-}

module Arkham.Strategy where

import Arkham.Card.CardDef
import Arkham.Id
import Arkham.Prelude
import Arkham.Target
import Arkham.Zone
import Control.Monad.Fail (fail)
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
  | ExileThis
  | RemoveThisFromGame
  | ShuffleThisBackIntoDeck
  | ReturnThisToHand
  | AbsoluteRemoveThisFromGame
  | DevourThis InvestigatorId
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
$(deriveToJSON defaultOptions ''AfterPlayStrategy)

instance FromJSON AfterPlayStrategy where
  parseJSON v = case v of
    String _ -> parseString v
    Object _ -> parseObject v
    _ -> fail "invalid AfterPlayStrategy"
   where
    parseString = withText "AfterPlayStrategy" \case
      "DiscardThis" -> pure DiscardThis
      "ExileThis" -> pure ExileThis
      "RemoveThisFromGame" -> pure RemoveThisFromGame
      "ShuffleThisBackIntoDeck" -> pure ShuffleThisBackIntoDeck
      "ReturnThisToHand" -> pure ReturnThisToHand
      "AbsoluteRemoveThisFromGame" -> pure AbsoluteRemoveThisFromGame
      _ -> fail "invalid AfterPlayStrategy"
    parseObject = withObject "AfterPlayStrategy" \o -> do
      tag <- o .: "tag"
      case tag :: Text of
        "DiscardThis" -> pure DiscardThis
        "ExileThis" -> pure ExileThis
        "RemoveThisFromGame" -> pure RemoveThisFromGame
        "ShuffleThisBackIntoDeck" -> pure ShuffleThisBackIntoDeck
        "ReturnThisToHand" -> pure ReturnThisToHand
        "AbsoluteRemoveThisFromGame" -> pure AbsoluteRemoveThisFromGame
        "DevourThis" -> DevourThis <$> o .: "investigatorId"
        _ -> fail "invalid AfterPlayStrategy"

$(deriveJSON defaultOptions ''ChosenCardStrategy)
