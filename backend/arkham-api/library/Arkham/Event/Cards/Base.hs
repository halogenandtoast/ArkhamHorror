module Arkham.Event.Cards.Base where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.Card.Cost
import Arkham.ClassSymbol
import Arkham.Criteria (Criterion, exists)
import Arkham.Criteria qualified as Criteria
import Arkham.Id
import Arkham.Matcher
import Arkham.Name
import Arkham.Prelude

canParallelRexClues :: InvestigatorMatcher
canParallelRexClues =
  InvestigatorIs "90078" <> InvestigatorWhenCriteria (Criteria.HasNRemainingCurseTokens (atLeast 2))

event :: CardCode -> Name -> Int -> ClassSymbol -> CardDef
event cardCode name cost = baseEvent cardCode name cost . singleton

baseEvent :: CardCode -> Name -> Int -> Set ClassSymbol -> CardDef
baseEvent cardCode name cost classSymbols =
  (emptyCardDef cardCode name EventType)
    { cdCost = Just (StaticCost cost)
    , cdClassSymbols = classSymbols
    }

multiClassEvent :: CardCode -> Name -> Int -> [ClassSymbol] -> CardDef
multiClassEvent cCode name cost classSymbols = baseEvent cCode name cost (setFromList classSymbols)

signature :: InvestigatorId -> CardDef -> CardDef
signature iid cd = cd {cdDeckRestrictions = [Signature iid], cdLevel = Nothing}

canDiscoverCluesAtYourLocation :: Criterion
canDiscoverCluesAtYourLocation =
  Criteria.Criteria
    [ exists $ YourLocation <> LocationWithAnyClues
    , exists $ You <> InvestigatorCanDiscoverCluesAt YourLocation
    ]
