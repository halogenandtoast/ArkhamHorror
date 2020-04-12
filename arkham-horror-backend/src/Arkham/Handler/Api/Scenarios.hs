module Arkham.Handler.Api.Scenarios where

import           Data.Map.Monoidal  (MonoidalMap)
import qualified Data.Map.Monoidal  as MonoidalMap
import           Database.Esqueleto
import           Import             hiding (on, (==.))

-- brittany-disable-next-binding

getApiV1ArkhamScenariosR :: Handler (MonoidalMap Int64 [Entity ArkhamHorrorScenario])
getApiV1ArkhamScenariosR = do
    groups <- runDB $ (convert <$$>) . select . from $ \(cycles `InnerJoin` scenarios) -> do
      on $ cycles ^.  ArkhamHorrorCycleId ==. scenarios ^.  ArkhamHorrorScenarioCycleId
      pure (cycles ^. ArkhamHorrorCycleId, scenarios)
    pure $ mconcat $ map (uncurry MonoidalMap.singleton) groups
 where
   convert = bimap (fromSqlKey . unValue) pure
   f <$$> a = (f <$>) <$> a

getApiV1ArkhamScenariosScenarioR
  :: ArkhamHorrorScenarioId -> Handler ArkhamHorrorScenario
getApiV1ArkhamScenariosScenarioR = runDB . get404
