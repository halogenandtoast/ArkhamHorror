module Base.Api.Handler.PasswordReset (postApiV1PasswordResetsR, putApiV1PasswordResetR) where

import Crypto.BCrypt
import Data.Aeson (withObject)
import Data.Text.Encoding qualified as TE
import Data.Time.Clock
import Import

newtype PasswordResetRequest = PasswordResetRequest {resetEmail :: Text}

instance FromJSON PasswordResetRequest where
  parseJSON = withObject "PasswordResetRequest" $ \o ->
    PasswordResetRequest <$> o .: "email"

data PasswordResetPassword = PasswordResetPassword {resetPassword :: Text}

instance FromJSON PasswordResetPassword where
  parseJSON = withObject "PasswordResetPassword" $ \o ->
    PasswordResetPassword <$> o .: "password"

postApiV1PasswordResetsR :: Handler ()
postApiV1PasswordResetsR = do
  payload <- requireCheckJsonBody
  resetExpiresAt <- liftIO $ addUTCTime nominalDay <$> getCurrentTime
  runDB $ do
    mUserId <- getBy (UniqueEmail $ resetEmail payload)
    for_ mUserId $ \(Entity userId _) ->
      insert_ $ PasswordReset userId resetExpiresAt

putApiV1PasswordResetR :: PasswordResetId -> Handler ()
putApiV1PasswordResetR resetId = do
  payload <- requireCheckJsonBody
  mdigest <-
    liftIO
      $ hashPasswordUsingPolicy
        slowerBcryptHashingPolicy
        (TE.encodeUtf8 $ resetPassword payload)
  now <- liftIO getCurrentTime

  runDB $ do
    reset <- get404 resetId
    if now > passwordResetExpiresAt reset
      then delete resetId
      else case mdigest of
        Nothing -> error "could not hash password"
        Just digest -> do
          update (passwordResetUserId reset) [UserPasswordDigest =. TE.decodeUtf8 digest]
          delete resetId
