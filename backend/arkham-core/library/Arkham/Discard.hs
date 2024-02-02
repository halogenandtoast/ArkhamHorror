module Arkham.Discard where

import Arkham.Prelude

import Arkham.Id
import Arkham.Matcher
import Arkham.Source
import Arkham.Target

data DiscardStrategy = DiscardChoose | DiscardRandom
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)

data HandDiscard msg = HandDiscard
  { discardStrategy :: DiscardStrategy
  , discardInvestigator :: InvestigatorId
  , discardSource :: Source
  , discardTarget :: Maybe Target
  , discardFilter :: CardMatcher
  , discardAmount :: Int
  , discardThen :: Maybe msg
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)
