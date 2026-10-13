{- | Where a filed bug's game export goes: a content-addressed object in the bug
bucket, whose URL the reporter pastes into the GitHub issue. Shared by the
Arkham and third-edition bug endpoints.
-}
module Api.BugUpload (uploadBugExport) where

import Amazonka (Error, discover, newEnv, runResourceT, send, toBody)
import Amazonka.S3 (ObjectKey (..), newHeadObject, newPutObject)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base16 qualified as B16
import Relude
import UnliftIO.Exception (catch)

{- | Upload the export under @<prefix><hash>.json@, skipping the write if that
exact export is already up. The prefix says which game it came from.
-}
uploadBugExport :: MonadIO m => Text -> LByteString -> m Text
uploadBugExport prefix jsonBody = do
  let
    filename = prefix <> decodeUtf8 (B16.encode (SHA256.hashlazy jsonBody)) <> ".json"
    bucket = "arkham-horror-bugs"
    key = ObjectKey ("exports/" <> filename)
  liftIO do
    env <- newEnv discover
    runResourceT do
      mResponse <-
        catch @_ @Error
          (Just <$> send env (newHeadObject bucket key))
          (\_ -> pure Nothing)
      whenNothing_ mResponse do
        void . send env $ newPutObject bucket key (toBody jsonBody)
  pure $ "https://arkham-horror-bugs.s3.amazonaws.com/exports/" <> filename
