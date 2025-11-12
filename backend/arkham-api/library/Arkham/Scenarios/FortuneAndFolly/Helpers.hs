{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Scenarios.FortuneAndFolly.Helpers (module Arkham.Scenarios.FortuneAndFolly.Helpers, module X) where

import Arkham.Ability.Types
import Arkham.Capability
import Arkham.Card.CardDef
import Arkham.Card.EncounterCard
import Arkham.Classes.GameLogger
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Message.Lifted
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers as X (
  getAlarmLevel,
  getMaxAlarmLevel,
  raiseAlarmLevel,
  reduceAlarmLevel,
  reduceAlarmLevelBy,
 )
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH
import Data.Function (on)

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "fortuneAndFolly" a

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Rank = Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data PlayingCard = PlayingCard
  { rank :: Rank
  , suit :: Suit
  }
  deriving stock (Ord, Eq, Show)

data CheckGameIcons = CheckGameIcons
  { target :: Target
  , investigator :: InvestigatorId
  , n :: Int
  , cards :: [EncounterCard]
  , mulligan :: Mulligan
  }
  deriving stock (Eq, Show)

data Mulligan = CanMulligan | NoMulligan
  deriving stock (Eq, Show)

foldMap (deriveJSON defaultOptions) [''Mulligan, ''Rank, ''Suit, ''PlayingCard, ''CheckGameIcons]

checkGameIcons
  :: (Targetable target, ReverseQueue m) => target -> InvestigatorId -> Mulligan -> Int -> m ()
checkGameIcons (toTarget -> target) iid mulligan n =
  scenarioSpecific "checkGameIcons"
    $ toJSON
    $ CheckGameIcons
      { cards = []
      , investigator = iid
      , mulligan
      , n
      , target
      }

sameRank :: HasCardDef a => Int -> [a] -> Bool
sameRank n cards =
  let playingCards = mapMaybe toPlayingCard cards
      rankGroups = groupBy (\a b -> a.rank == b.rank) (sortBy (compare `on` (.rank)) playingCards)
   in any (\grp -> length grp >= n) rankGroups

allSameSuit :: HasCardDef a => [a] -> Bool
allSameSuit cards = case mapMaybe toPlayingCard cards of
  [] -> False
  (x : xs) -> all (\pc -> pc.suit == x.suit) xs

allSameRank :: HasCardDef a => [a] -> Bool
allSameRank cards = case mapMaybe toPlayingCard cards of
  [] -> False
  (x : xs) -> all (\pc -> pc.rank == x.rank) xs

sequential :: HasCardDef a => [a] -> Bool
sequential cards =
  let playingCards = mapMaybe toPlayingCard cards
      sortedRanks = sort $ map rankValue playingCards
   in and $ zipWith (\a b -> b == a + 1) sortedRanks (drop 1 sortedRanks)

toPlayingCard :: HasCardDef a => a -> Maybe PlayingCard
toPlayingCard a = PlayingCard <$> rank <*> suit
 where
  cardDef = toCardDef a
  suit = toSuit =<< lookup "suit" (cdMeta cardDef)
  toSuit = \case
    "hearts" -> Just Hearts
    "diamonds" -> Just Diamonds
    "clubs" -> Just Clubs
    "spades" -> Just Spades
    _ -> Nothing
  rank = toRank =<< lookup "value" (cdMeta cardDef)
  toRank = \case
    "four" -> Just Four
    "five" -> Just Five
    "six" -> Just Six
    "seven" -> Just Seven
    "eight" -> Just Eight
    "nine" -> Just Nine
    "ten" -> Just Ten
    "jack" -> Just Jack
    "queen" -> Just Queen
    "king" -> Just King
    "ace" -> Just Ace
    _ -> Nothing

rankValue :: PlayingCard -> Int
rankValue pc = case pc.rank of
  Four -> 4
  Five -> 5
  Six -> 6
  Seven -> 7
  Eight -> 8
  Nine -> 9
  Ten -> 10
  Jack -> 11
  Queen -> 12
  King -> 13
  Ace -> 14

numericValue :: PlayingCard -> Int
numericValue pc = case pc.rank of
  Four -> 4
  Five -> 5
  Six -> 6
  Seven -> 7
  Eight -> 8
  Nine -> 9
  Ten -> 10
  Jack -> 10
  Queen -> 10
  King -> 10
  Ace -> 10

winGame
  :: (HasGameLogger m, ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
winGame iid (toSource -> source) n = do
  sendUI "confetti"
  whenM (can.gain.resources iid) do
    abilityModifier (AbilityRef source 1) (AbilitySource source 1) iid (ScenarioModifier "gotResources")
    gainResources iid source n
