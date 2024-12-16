{-# LANGUAGE TemplateHaskell #-}

module Arkham.Search where

import Arkham.Card
import Arkham.Id
import Arkham.Matcher.Card
import Arkham.Prelude
import Arkham.Source
import Arkham.Strategy
import Arkham.Target
import Arkham.Zone
import Control.Monad.Fail (fail)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Map.Strict qualified as Map
import Data.Vector ((!?))
import GHC.Records

data SearchType = Searching | Looking | Revealing
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data Search = MkSearch
  { searchType :: SearchType
  , searchInvestigator :: InvestigatorId
  , searchSource :: Source
  , searchTarget :: Target
  , searchZones :: [(Zone, ZoneReturnStrategy)]
  , searchMatcher :: ExtendedCardMatcher
  , searchFoundStrategy :: FoundCardsStrategy
  , searchFoundCards :: Map Zone [Card]
  , searchDrawnCards :: [Card]
  }
  deriving stock (Show, Eq, Data)

mkSearch
  :: (Sourceable source, Targetable target, AsId investigator, IdOf investigator ~ InvestigatorId)
  => SearchType
  -> investigator
  -> source
  -> target
  -> [(Zone, ZoneReturnStrategy)]
  -> ExtendedCardMatcher
  -> FoundCardsStrategy
  -> Search
mkSearch sType investigator source target zones matcher strategy =
  MkSearch
    sType
    (asId investigator)
    (toSource source)
    (toTarget target)
    zones
    matcher
    strategy
    mempty
    mempty

foundCardsL :: Lens' Search (Map Zone [Card])
foundCardsL = lens searchFoundCards $ \m x -> m {searchFoundCards = x}

drawnCardsL :: Lens' Search [Card]
drawnCardsL = lens searchDrawnCards $ \m x -> m {searchDrawnCards = x}

instance HasField "foundCards" Search (Map Zone [Card]) where
  getField = searchFoundCards

instance HasField "allFoundCards" Search [Card] where
  getField = concat . Map.elems . searchFoundCards

deriveToJSON defaultOptions ''Search

(.!) :: FromJSON a => Vector Value -> Int -> Parser a
(.!) v i = case v !? i of
  Just a -> parseJSON a
  Nothing -> fail $ "Expected a Vector with at least " <> show i <> " elements"

instance FromJSON Search where
  parseJSON (Array v) = do
    searchType <- v .! 0
    searchInvestigator <- v .! 1
    searchSource <- v .! 2
    searchTarget <- v .! 3
    searchZones <- v .! 4
    searchMatcher <- v .! 5
    searchFoundStrategy <- v .! 6
    searchFoundCards <- case v !? 7 of
      Just a -> parseJSON a
      Nothing -> pure mempty
    searchDrawnCards <- case v !? 8 of
      Just a -> parseJSON a
      Nothing -> pure mempty
    pure MkSearch {..}
  parseJSON (Object o) = do
    searchType <- o .: "searchType" <|> o .: "searchingType"
    searchInvestigator <- o .: "searchInvestigator" <|> o .: "searchingInvestigator"
    searchSource <- o .: "searchSource" <|> o .: "searchingSource"
    searchTarget <- o .: "searchTarget" <|> o .: "searchingTarget"
    searchZones <- o .: "searchZones" <|> o .: "searchingZones"
    searchMatcher <- o .: "searchMatcher" <|> o .: "searchingMatcher"
    searchFoundStrategy <- o .: "searchFoundStrategy" <|> o .: "searchingFoundCardsStrategy"
    searchFoundCards <- o .: "searchFoundCards" <|> o .: "searchingFoundCards" <|> pure mempty
    searchDrawnCards <- o .: "searchDrawnCards" <|> o .: "searchingDrawnCards" <|> pure mempty
    pure MkSearch {..}
  parseJSON v = fail $ "Expected an array or an object, but found: " <> show v
