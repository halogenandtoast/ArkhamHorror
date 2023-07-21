module Base.Api.Handler.Registration where

import Crypto.BCrypt
import Data.Text.Encoding qualified as TE
import Import
import Types

registrationToUser :: Registration -> Handler User
registrationToUser Registration {..} = do
  mdigest <-
    liftIO $
      hashPasswordUsingPolicy
        slowerBcryptHashingPolicy
        (TE.encodeUtf8 registrationPassword)
  case mdigest of
    Nothing -> error "could not hash password"
    Just digest ->
      pure $
        User
          registrationUsername
          registrationEmail
          (TE.decodeUtf8 digest)
          False

postApiV1RegistrationR :: Handler Token
postApiV1RegistrationR = do
  user <- requireCheckJsonBody >>= registrationToUser
  userId <- runDB $ insert user
  Token <$> userIdToToken userId
