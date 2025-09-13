module Api.Handler.Arkham.Game.Bug (postApiV1ArkhamGameBugR) where

import Amazonka
import Amazonka.S3
import Api.Arkham.Export
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (encode)
import Data.ByteString.Base16 qualified as B16
import Import hiding ((==.))
import UnliftIO.Exception (catch)

postApiV1ArkhamGameBugR :: ArkhamGameId -> Handler Text
postApiV1ArkhamGameBugR gameId = do
  _ <- getRequestUserId
  export <- generateExport gameId

  let jsonBody = encode export
  let filename = decodeUtf8 (B16.encode $ SHA256.hashlazy jsonBody) <> ".json"

  -- Set S3 parameters
  let bucket = "arkham-horror-bugs"
      key = ObjectKey $ "exports/" <> filename

  liftIO do
    -- Initialize AWS environment
    env <- newEnv discover
    runResourceT do
      mResponse <-
        catch @_ @Error
          (Just <$> send env (newHeadObject bucket key))
          (\_ -> pure Nothing)

      whenNothing_ mResponse do
        void . send env $ newPutObject bucket key $ toBody jsonBody

  pure $ "https://arkham-horror-bugs.s3.amazonaws.com/exports/" <> filename
