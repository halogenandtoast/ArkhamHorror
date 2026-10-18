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
  runDB do
    User {..} <- get404 userId
    pure
      $ CurrentUser
        userUsername
        userEmail
        userBeta
        userPhaseTransitionNotifications
        userAdmin
        userAllowSpectatorsByDefault
        (Feature.parseFeatures userFeatures)
