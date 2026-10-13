module Api.Handler.Arkham.Game.Bug (postApiV1ArkhamGameBugR) where

import Api.Arkham.Export
import Api.BugUpload (uploadBugExport)
import Data.Aeson (encode)
import Import hiding ((==.))

postApiV1ArkhamGameBugR :: ArkhamGameId -> Handler Text
postApiV1ArkhamGameBugR gameId = do
  _ <- getRequestUserId
  export <- generateScenarioExport gameId
  uploadBugExport "" (encode export)
