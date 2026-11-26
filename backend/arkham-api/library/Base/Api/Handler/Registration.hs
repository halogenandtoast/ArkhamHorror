module Base.Api.Handler.Registration where

import Crypto.BCrypt
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (getCurrentTime)
import Import
import Types

registrationToUser :: Registration -> Handler User
registrationToUser Registration {..} = do
  -- Creating the account is the first login and the first sighting: it hands
  -- back a token and the client is signed in on it, so both clocks start here
  -- rather than at whatever the columns' defaults happened to be.
  now <- liftIO getCurrentTime
  mdigest <-
    liftIO
      $ hashPasswordUsingPolicy
        slowerBcryptHashingPolicy
        (TE.encodeUtf8 registrationPassword)
  case mdigest of
    Nothing -> error "could not hash password"
    Just digest ->
      pure
        $ User
          registrationUsername
          registrationEmail
          (TE.decodeUtf8 digest)
          False
          False
          False
          ""
          False
          now
          now

{- | Refuse before the password is hashed or the database is touched, so a
closed site costs a bcrypt round per attempt instead of one per request.

403 rather than 404: the route exists and the client is told why, which is what
lets the sign-up form distinguish "registration is closed" from the name or
email already being taken. 'Settings.appInviteOnly' also reaches the client
through @GET /api/v1/site-settings@, so the form is normally never offered --
this is the guard for a bookmark, a stale tab, or a direct POST.
-}
postApiV1RegistrationR :: Handler Token
postApiV1RegistrationR = do
  inviteOnly <- getsApp (appInviteOnly . appSettings)
  when inviteOnly $ permissionDenied "Registration is closed"
  user <- requireCheckJsonBody >>= registrationToUser
  userId <- runDB $ insert user
  Token <$> userIdToToken userId
