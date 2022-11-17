module Arkham.Scenarios.TheDepthsOfYoth.Helpers where

import Arkham.Prelude
import Arkham.Helpers.Scenario
import Arkham.Scenario.Types (Field(..))
import {-# SOURCE #-} Arkham.GameEnv
import Data.Aeson (Result(..))

newtype DepthsOfYothMeta = DepthsOfYothMeta { currentDepth :: Int }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

getCurrentDepth :: (HasGame m, Monad m) => m Int
getCurrentDepth = do
  v <- scenarioField ScenarioMeta
  pure $ case fromJSON v of
    Error _ -> error "invalid meta for depths of yoth"
    Success a -> currentDepth a

toMeta :: Int -> Value
toMeta = toJSON . DepthsOfYothMeta
