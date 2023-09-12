module Arkham.Scenarios.BeforeTheBlackThrone.Helpers where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Source
import Arkham.Target
import Arkham.Trait (Trait (Cultist))
import Data.Aeson (Result (..))

getCosmos :: HasGame m => m (Cosmos Card LocationId)
getCosmos = do
  cosmos' <- scenarioField ScenarioMeta
  case fromJSON cosmos' of
    Error e -> error $ "failed to parse cosmos: " <> e
    Success result -> pure result

findCosmosPosition :: HasGame m => InvestigatorId -> m (Maybe Pos)
findCosmosPosition iid = do
  cosmos' <- getCosmos
  lid <- getJustLocation iid
  pure $ findInCosmos lid cosmos'

cosmosFail :: HasQueue Message m => LocationAttrs -> m ()
cosmosFail attrs = do
  pushAll
    [ RemoveFromGame (toTarget attrs)
    , ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey CosmosDeck) [toCard attrs]
    ]

getEmptyPositionsInDirections :: HasGame m => InvestigatorId -> [GridDirection] -> m [Pos]
getEmptyPositionsInDirections iid directions = do
  cosmos' <- getCosmos
  mpos <- findCosmosPosition iid
  case mpos of
    Nothing -> pure []
    Just pos -> do
      let adjacents = positionsInDirections pos directions
      pure $ filter (\adj -> isEmpty $ viewCosmos adj cosmos') adjacents

getLocationInDirection :: HasGame m => LocationId -> GridDirection -> m (Maybe LocationId)
getLocationInDirection lid dir = do
  cosmos' <- getCosmos
  pure $ case findInCosmos lid cosmos' of
    Nothing -> Nothing
    Just pos -> case viewCosmos (updatePosition pos dir) cosmos' of
      Nothing -> Nothing
      Just (EmptySpace _ _) -> Nothing
      Just (CosmosLocation _ lid') -> Just lid'

getCanMoveLocationLeft :: HasGame m => LocationId -> m Bool
getCanMoveLocationLeft lid = do
  cosmos' <- getCosmos
  pure $ case findInCosmos lid cosmos' of
    Nothing -> False -- could be player location, like Luke
    Just pos -> case viewCosmos (updatePosition pos GridLeft) cosmos' of
      Nothing -> True
      Just (EmptySpace _ _) -> True
      Just (CosmosLocation _ _) -> False

commitRitualSuicide :: (HasGame m, Sourceable source) => source -> m [Message]
commitRitualSuicide (toSource -> source) = do
  cultists <- selectList $ EnemyWithTrait Cultist
  doom <- getSum <$> foldMapM (fieldMap EnemyDoom Sum) cultists
  azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
  pure
    $ map (Discard source . toTarget) cultists
      <> [PlaceDoom source (toTarget azathoth) doom]
