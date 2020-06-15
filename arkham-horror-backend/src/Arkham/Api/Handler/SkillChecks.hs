module Arkham.Api.Handler.SkillChecks where

import Import
import Arkham.Types
import Arkham.Fixtures
import System.Random
import qualified Data.List.NonEmpty as NE

getApiV1ArkhamSkillCheckR :: Int -> Handler ArkhamChaosToken
getApiV1ArkhamSkillCheckR gameId = do
  ArkhamGame {..} <- liftIO $ loadGameFixture gameId
  liftIO $ drawFromBag $ chaosBag gameState
 where
  drawFromBag xs = (xs NE.!!) <$> randomRIO (0, NE.length xs - 1)

   
