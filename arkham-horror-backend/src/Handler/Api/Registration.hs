module Handler.Api.Registration where

import           Arkham.Types
import           Crypto.BCrypt
import qualified Data.Text.Encoding as TE
import           Import

data Registration = Registration
    { email    :: Text
    , username :: Text
    , password :: Text
    }
    deriving stock (Generic)

instance FromJSON Registration

registrationToUser :: Registration -> Handler User
registrationToUser Registration {..} = do
  mdigest <- liftIO
    $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (TE.encodeUtf8 password)
  case mdigest of
    Nothing     -> error "could not hash password"
    Just digest -> pure $ User username email (TE.decodeUtf8 digest)

postApiV1RegistrationR :: Handler Token
postApiV1RegistrationR = do
  user   <- requireCheckJsonBody >>= registrationToUser
  userId <- runDB $ insert user
  Token <$> userIdToToken userId
