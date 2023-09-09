module Arkham.Scenarios.BeforeTheBlackThrone.Helpers where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.HasQueue
import Arkham.Deck qualified as Deck
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Location.Types
import Arkham.Message
import Arkham.Scenario.Deck
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Target
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

cosmosFail :: HasQueue Message m => LocationAttrs -> m ()
cosmosFail attrs = do
  pushAll
    [ RemoveFromGame (toTarget attrs)
    , ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey CosmosDeck) [toCard attrs]
    ]

getEmptyPositionsInDirections :: HasGame m => InvestigatorId -> [GridDirection] -> m [Pos]
getEmptyPositionsInDirections iid directions = do
  cosmos' <- getCosmos
  pos <- findCosmosPosition iid
  let adjacents = positionsInDirections pos directions
  pure $ filter (\adj -> isEmpty $ viewCosmos adj cosmos') adjacents
