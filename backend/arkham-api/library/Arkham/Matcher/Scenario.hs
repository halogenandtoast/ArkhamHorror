{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Scenario where

import {-# SOURCE #-} Arkham.Modifier
import Arkham.Prelude
import Arkham.Token
import Data.Aeson.TH

data ScenarioMatcher = TheScenario | ScenarioWithModifier ModifierType | ScenarioWithToken Token
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''ScenarioMatcher)
