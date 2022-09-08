module Arkham.Criteria.Override where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Criteria

newtype CriteriaOverride = CriteriaOverride Criterion
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
