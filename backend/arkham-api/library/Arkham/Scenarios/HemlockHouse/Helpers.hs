module Arkham.Scenarios.HemlockHouse.Helpers where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Classes.HasGame (HasGame)
import {-# SOURCE #-} Arkham.Game.Utils (maybeEnemyLocation)
import Arkham.Helpers.Message (push)
import Arkham.Helpers.Scenario (getGrid)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Grid (GridLocation (..), Pos (..), findInGrid, flattenGrid)
import Arkham.Location.Types (Field (..))
import Arkham.Message (Message (FlipToEnemyLocation, FlipToLocation))
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Token (Token (..), countTokens)
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "hemlockHouse" a

{- | Floor number for a location from its grid Y position.
1st Floor (Parlor/Foyer/Dining) is Y=0 → floor 1.
The Cellar (Shapeless Cellar enemy-location) is at Y<0 → floor 0.
-}
getFloorNumber :: (HasGame m, Tracing m) => LocationId -> m Int
getFloorNumber lid = do
  mPos <- field LocationPosition lid
  case mPos of
    Just (Pos _ y) | y >= 0 -> pure (y + 1)
    _ -> pure 0

{- | Number of seals on a location.
Seals are modeled as Resource tokens placed by the act/agenda's seal action
("Place 1 resource on it, as a seal").
-}
locationSealCount :: (HasGame m, Tracing m) => LocationId -> m Int
locationSealCount lid = do
  ts <- field LocationTokens lid
  pure $ countTokens Resource ts

locationIsUnsealed :: (HasGame m, Tracing m) => LocationId -> m Bool
locationIsUnsealed lid = (== 0) <$> locationSealCount lid

{- | Flip a Hemlock location to its other side, regardless of which side it is
currently on. Tokens (and other location state) are retained via the
FlipToEnemyLocation/FlipToLocation handlers.
-}
flipLocationOver :: ReverseQueue m => LocationId -> m ()
flipLocationOver lid = do
  card <- field LocationCard lid
  isEnemyLoc <- isJust <$> maybeEnemyLocation lid
  push $ if isEnemyLoc then FlipToLocation lid card else FlipToEnemyLocation lid card

-- | Manhattan distance between two grid positions.
manhattan :: Pos -> Pos -> Int
manhattan (Pos x1 y1) (Pos x2 y2) = abs (x1 - x2) + abs (y1 - y2)

{- | Find the nearest enemy-location to the given investigator on the grid.
Each location in the grid is adjacent to its orthogonal neighbors, so
Manhattan distance matches the rules' "nearest" relation here.
-}
nearestEnemyLocationTo
  :: (HasGame m, Tracing m) => InvestigatorId -> m (Maybe LocationId)
nearestEnemyLocationTo iid = do
  mStart <- field InvestigatorLocation iid
  case mStart of
    Nothing -> pure Nothing
    Just start -> do
      grid <- getGrid
      case findInGrid start grid of
        Nothing -> pure Nothing
        Just startPos -> do
          let allLocs = [(lid, p) | GridLocation p lid <- flattenGrid grid]
          enemyLocs <- filterM (fmap isJust . maybeEnemyLocation . fst) allLocs
          let withDist = [(manhattan startPos p, lid) | (lid, p) <- enemyLocs]
          pure $ case sortOn fst withDist of
            [] -> Nothing
            (_, lid) : _ -> Just lid
