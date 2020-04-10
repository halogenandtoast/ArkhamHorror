module Handler.Api.Arkham.Scenarios where

import           Arkham.Types
import qualified Data.Map.Strict    as Map
import           Database.Esqueleto
import           Import             hiding (Value, groupBy, on, (==.))

unValue2 :: (Value a, Value b) -> (a, b)
unValue2 (a, b) = (unValue a, unValue b)

-- brittany-disable-next-binding

getApiV1ArkhamScenariosR :: Handler (Map Cycle [Scenario])
getApiV1ArkhamScenariosR = do
  matches <- runDB $ map unValue2 <$> select . from $ \(cycles `InnerJoin` scenarios) -> do
    on $ just (cycles ^.  ArkhamHorrorCycleId) ==. scenarios ^.  ArkhamHorrorScenarioCycleId
    groupBy $ cycles ^. ArkhamHorrorCycleId
    pure (cycles ^. ArkhamHorrorCycleName, scenarios ^. ArkhamHorrorScenarioName)
  pure $ Map.fromList matches
