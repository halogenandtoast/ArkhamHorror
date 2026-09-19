{-# LANGUAGE DuplicateRecordFields #-}

module Base.Api.Handler.Settings where

import Database.Esqueleto.Experimental
import Import hiding (update, (=.), (==.))

data UserSettings = UserSettings
  { beta :: Maybe Bool
  , phaseTransitionNotifications :: Maybe Bool
  }
  deriving stock Generic
  deriving anyclass FromJSON

data CurrentUser = CurrentUser
  { username :: Text
  , email :: Text
  , beta :: Bool
  , phaseTransitionNotifications :: Bool
  }
  deriving stock Generic
  deriving anyclass ToJSON

newtype SiteSettings = SiteSettings
  { assetHost :: Maybe Text
  }

instance ToJSON SiteSettings where
  toJSON SiteSettings {assetHost} = object ["assetHost" .= assetHost]

getApiV1SiteSettingsR :: Handler SiteSettings
getApiV1SiteSettingsR = SiteSettings <$> getsApp (appAssetHost . appSettings)

putApiV1SettingsR :: Handler CurrentUser
putApiV1SettingsR = do
  userId <- getRequestUserId
  settings <- requireCheckJsonBody
  runDB do
    let UserSettings mBeta mPhaseTransitionNotifications = settings
    update \u -> do
      for_ mBeta \value -> set u [UserBeta =. val value]
      for_ mPhaseTransitionNotifications \value ->
        set u [UserPhaseTransitionNotifications =. val value]
      where_ $ u.id ==. val userId
    User { userUsername, userEmail, userBeta, userPhaseTransitionNotifications } <- get404 userId
    pure $ CurrentUser userUsername userEmail userBeta userPhaseTransitionNotifications
