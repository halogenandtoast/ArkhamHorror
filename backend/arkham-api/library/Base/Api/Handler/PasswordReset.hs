module Base.Api.Handler.PasswordReset (postApiV1PasswordResetsR, putApiV1PasswordResetR) where

import Crypto.BCrypt
import Data.Aeson (withObject)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock
import Data.UUID qualified as UUID (toText)
import Import
import Network.Mail.Mailtrap
import Text.Email.Parser (unsafeEmailAddress)

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
  Entity userId _ <- runDB $ getBy404 (UniqueEmail $ resetEmail payload)
  token <- runDB $ insert $ PasswordReset userId resetExpiresAt
  sendPasswordResetEmail (resetEmail payload) (UUID.toText $ coerce token)
 where
  sendPasswordResetEmail :: Text -> Text -> Handler ()
  sendPasswordResetEmail email token = do
    case parseEmailAddress (TE.encodeUtf8 email) of
      Left _ -> pure ()
      Right emailParsed -> do
        let url = "https://arkhamhorror.app/#/password-reset/" <> token
        apiToken <- getsYesod $ appMailtrapApiToken . appSettings
        liftIO $ print apiToken
        void
          $ liftIO
          $ sendEmail apiToken
          $ Email
            { email_from =
                NamedEmailAddress (unsafeEmailAddress "noreply" "arkhamhorror.app") "No Reply"
            , email_to = [NamedEmailAddress emailParsed ""]
            , email_cc = []
            , email_bcc = []
            , email_attachments = []
            , email_custom = mempty
            , email_message =
                Right
                  $ Message
                    { message_subject = "Password Reset for Arkham Horror Online"
                    , message_body =
                        PlainTextBody
                          $ T.unlines
                            [ "You have requested a password reset."
                            , ""
                            , "To reset your password, click the link below:"
                            , ""
                            , url
                            , ""
                            , "If you did not request a password reset, you can safely ignore this email."
                            ]
                    , message_category = "Password Reset"
                    }
            }

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
