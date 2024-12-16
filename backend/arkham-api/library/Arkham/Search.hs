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
import Data.Aeson.TH
import Data.Map.Strict qualified as Map
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

deriveJSON defaultOptions ''Search
