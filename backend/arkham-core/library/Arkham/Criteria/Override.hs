module Arkham.Criteria.Override where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Criteria

data CriteriaOverride = CriteriaOverride
  { originalCriteria :: Criterion
  , replacementCriteria :: Criterion
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
