{-# LANGUAGE TemplateHaskell #-}

module Arkham.Criteria.Override where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Criteria
import Data.Aeson.TH

newtype CriteriaOverride = CriteriaOverride Criterion
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (NoThunks, NFData)

$(deriveJSON defaultOptions ''CriteriaOverride)
