{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Scenario where

import {-# SOURCE #-} Arkham.Modifier
import Arkham.Prelude
import Data.Aeson.TH

data ScenarioMatcher = TheScenario | ScenarioWithModifier ModifierType
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''ScenarioMatcher)
