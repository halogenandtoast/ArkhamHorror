{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Base.Api.Handler.CurrentUser where

import Auth.Feature qualified as Feature
import Import

data CurrentUser = CurrentUser
  { username :: Text
  , email :: Text
  , beta :: Bool
  , phaseTransitionNotifications :: Bool
  , admin :: Bool
  , allowSpectatorsByDefault :: Bool
  -- ^ what the new-game form's "allow spectators" switch starts as.
  , features :: [Feature.Feature]
  {- ^ What this account has been let into, so the client shows only the panels
  the server will actually serve. The gate itself is 'Foundation.isAuthorized'.
  -}
  }
  deriving stock Generic
  deriving anyclass ToJSON

getApiV1CurrentUserR :: Handler CurrentUser
getApiV1CurrentUserR = do
  userId <- getRequestUserId
  user <- runDB $ get404 userId
  {- Resuming a session on a stored token is being seen, so this is where an
  account in continuous use keeps its place -- and where a dormant one is turned
  away while the server is invite only. A login-only check could be sat out
  indefinitely by simply never logging in again, the token having no expiry. -}
  admitToSession ResumedOnToken (Entity userId user)
  pure
    $ CurrentUser
      user.username
      user.email
      user.beta
      user.phaseTransitionNotifications
      user.admin
      user.allowSpectatorsByDefault
      (Feature.parseFeatures user.features)
