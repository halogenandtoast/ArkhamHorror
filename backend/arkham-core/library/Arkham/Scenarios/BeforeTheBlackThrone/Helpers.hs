module Arkham.Scenarios.BeforeTheBlackThrone.Helpers where

import Arkham.Prelude

import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Data.Aeson (Result (..))

getCosmos :: HasGame m => m (Cosmos Card LocationId)
getCosmos = do
  cosmos' <- scenarioField ScenarioMeta
  case fromJSON cosmos' of
    Error e -> error $ "failed to parse cosmos: " <> e
    Success result -> pure result

findCosmosPosition :: HasGame m => InvestigatorId -> m Pos
findCosmosPosition iid = do
  cosmos' <- getCosmos
  lid <- getJustLocation iid
  case findInCosmos lid cosmos' of
    Nothing -> error "failed to find cosmos location"
    Just pos -> pure pos
