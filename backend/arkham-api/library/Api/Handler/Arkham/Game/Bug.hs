module Api.Handler.Arkham.Game.Bug (
  postApiV1ArkhamGameBugR,
) where

import Amazonka
import Amazonka.S3
import Api.Arkham.Export
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (encode)
import Data.ByteString.Base16 qualified as B16
import Import hiding ((==.))
import Safe (fromJustNote)
import UnliftIO.Exception (catch)

uploadJsonToS3 :: ToJSON a => a -> Handler Text
uploadJsonToS3 jsonData = do
  let jsonBody = encode jsonData
  let filename = decodeUtf8 (B16.encode $ SHA256.hashlazy jsonBody) <> ".json"

  -- Set S3 parameters
  let bucket = BucketName "arkham-horror-bugs"
      key = ObjectKey $ "exports/" <> filename

  -- Initialize AWS environment
  env <- liftIO $ newEnv discover

  void $ liftIO $ runResourceT $ do
    mResponse <-
      catch @_ @Error
        (Just <$> send env (newHeadObject bucket key))
        (\_ -> pure Nothing)

    case mResponse of
      Just _ -> pure () -- already exists
      Nothing ->
        void . send env $ newPutObject bucket key $ toBody jsonBody

  pure $ "https://arkham-horror-bugs.s3.amazonaws.com/exports/" <> filename

postApiV1ArkhamGameBugR :: ArkhamGameId -> Handler Text
postApiV1ArkhamGameBugR gameId = do
  _ <- fromJustNote "Not authenticated" <$> getRequestUserId
  uploadJsonToS3 =<< generateExport gameId
