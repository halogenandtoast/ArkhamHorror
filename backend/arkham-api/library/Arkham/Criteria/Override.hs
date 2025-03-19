{-# LANGUAGE TemplateHaskell #-}

module Arkham.Criteria.Override where

import Arkham.Prelude
import {-# SOURCE #-} Arkham.Criteria
import Data.Aeson.TH
import Arkham.Matcher.Base

newtype CriteriaOverride = CriteriaOverride Criterion
  deriving stock (Show, Eq, Ord, Data)

combineOverrides :: NonEmpty CriteriaOverride -> CriteriaOverride
combineOverrides (x :| xs) = CriteriaOverride $ oneOf $ coerce (x : xs)

$(deriveJSON defaultOptions ''CriteriaOverride)
