{-# LANGUAGE DuplicateRecordFields #-}

module Base.Api.Handler.Settings where

import Database.Esqueleto.Experimental
import Import hiding (update, (=.), (==.))

data UserSettings = UserSettings
  { beta :: Maybe Bool
  , phaseTransitionNotifications :: Maybe Bool
  , allowSpectatorsByDefault :: Maybe Bool
  }
  deriving stock Generic
  deriving anyclass FromJSON

data CurrentUser = CurrentUser
  { username :: Text
  , email :: Text
  , beta :: Bool
  , phaseTransitionNotifications :: Bool
  , allowSpectatorsByDefault :: Bool
  {- ^ what the "allow spectators" switch starts as on the new-game form. Only
  a starting point: the form still sends the value it was left at.
  -}
  }
  deriving stock Generic
  deriving anyclass ToJSON

data SiteSettings = SiteSettings
  { assetHost :: Maybe Text
  , maintenance :: Bool
  }

instance ToJSON SiteSettings where
  toJSON SiteSettings {assetHost, maintenance} =
    object ["assetHost" .= assetHost, "maintenance" .= maintenance]

{- | Allowed through 'Foundation.maintenanceMiddleware', and so the one thing
the client can still ask during a window. It reads no database -- the asset
host is a setting and the flag is a 'TVar' -- which is what makes it safe to
leave open.
-}
getApiV1SiteSettingsR :: Handler SiteSettings
getApiV1SiteSettingsR = do
  assetHost <- getsApp (appAssetHost . appSettings)
  maintenance <- isJust <$> (getsApp appMaintenance >>= readTVarIO)
  pure SiteSettings {..}

putApiV1SettingsR :: Handler CurrentUser
putApiV1SettingsR = do
  userId <- getRequestUserId
  settings <- requireCheckJsonBody
  runDB do
    let UserSettings mBeta mPhaseTransitionNotifications mAllowSpectatorsByDefault = settings
    update \u -> do
      for_ mBeta \value -> set u [UserBeta =. val value]
      for_ mPhaseTransitionNotifications \value ->
        set u [UserPhaseTransitionNotifications =. val value]
      for_ mAllowSpectatorsByDefault \value ->
        set u [UserAllowSpectatorsByDefault =. val value]
      where_ $ u.id ==. val userId
    User
      { userUsername
      , userEmail
      , userBeta
      , userPhaseTransitionNotifications
      , userAllowSpectatorsByDefault
      } <-
      get404 userId
    pure
      $ CurrentUser
        userUsername
        userEmail
        userBeta
        userPhaseTransitionNotifications
        userAllowSpectatorsByDefault
