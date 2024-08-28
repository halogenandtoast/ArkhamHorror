module Arkham.Discard where

import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import GHC.Records

data DiscardStrategy = DiscardChoose | DiscardRandom | DiscardAll
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data HandDiscard msg = HandDiscard
  { discardStrategy :: DiscardStrategy
  , discardInvestigator :: InvestigatorId
  , discardSource :: Source
  , discardTarget :: Maybe Target
  , discardFilter :: CardMatcher
  , discardAmount :: Int
  , discardThen :: Maybe msg
  }
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "strategy" (HandDiscard msg) DiscardStrategy where
  getField = discardStrategy
  {-# INLINE getField #-}

instance HasField "investigator" (HandDiscard msg) InvestigatorId where
  getField = discardInvestigator
  {-# INLINE getField #-}

instance HasField "source" (HandDiscard msg) Source where
  getField = discardSource
  {-# INLINE getField #-}

instance HasField "target" (HandDiscard msg) (Maybe Target) where
  getField = discardTarget
  {-# INLINE getField #-}

instance HasField "filter" (HandDiscard msg) CardMatcher where
  getField = discardFilter
  {-# INLINE getField #-}

instance HasField "amount" (HandDiscard msg) Int where
  getField = discardAmount
  {-# INLINE getField #-}

instance HasField "then" (HandDiscard msg) (Maybe msg) where
  getField = discardThen
  {-# INLINE getField #-}
