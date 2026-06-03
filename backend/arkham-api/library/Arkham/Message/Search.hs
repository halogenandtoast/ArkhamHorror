{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Search where

import Arkham.Card (Card)
import Arkham.Deck (DeckSignifier)
import Arkham.Id
import Arkham.Matcher (CardMatcher)
import Arkham.Prelude
import Arkham.Search (Search)
import Arkham.Strategy (ZoneReturnStrategy)
import Arkham.Source (Source)
import Arkham.Target (Target)
import Arkham.Zone (Zone)
import Data.Aeson.TH

-- | Messages dealing with deck/zone search lifecycle: starting a search,
-- discovering candidates, choosing among them, and ending the search.
data SearchMessage
  = Search_ Search
  | ResolveSearch_ Target
  | FinishedSearch_
  | PreSearchFound_ InvestigatorId (Maybe Target) DeckSignifier [Card]
  | SearchFound_ InvestigatorId Target DeckSignifier [Card]
  | FoundCards_ (Map Zone [Card])
  | SearchNoneFound_ InvestigatorId Target
  | UpdateSearchReturnStrategy_ InvestigatorId Zone ZoneReturnStrategy
  | SearchCollectionForRandom_ InvestigatorId Source CardMatcher
  | SearchEnded_ Target
  | CancelSearch_ Target
  | ClearFound_ Zone
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''SearchMessage)
