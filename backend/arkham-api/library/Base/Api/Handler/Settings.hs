{-# LANGUAGE DuplicateRecordFields #-}

module Base.Api.Handler.Settings where

import Database.Esqueleto.Experimental
import Import hiding (update, (=.), (==.))

newtype UserSettings = UserSettings {beta :: Bool}
  deriving stock Generic
  deriving anyclass FromJSON

betaSetting :: UserSettings -> Bool
betaSetting (UserSettings b) = b

data CurrentUser = CurrentUser
  { username :: Text
  , email :: Text
  , beta :: Bool
  }
  deriving stock Generic
  deriving anyclass ToJSON

data SiteSettings = SiteSettings
  { assetHost :: Maybe Text
  , audioHost :: Maybe Text
  }

instance ToJSON SiteSettings where
  toJSON SiteSettings {assetHost, audioHost} = object ["assetHost" .= assetHost, "audioHost" .= audioHost]

getApiV1SiteSettingsR :: Handler SiteSettings
getApiV1SiteSettingsR = do
  settings <- getsApp appSettings
  pure $ SiteSettings settings.appAssetHost settings.appAudioHost

putApiV1SettingsR :: Handler CurrentUser
putApiV1SettingsR = do
  userId <- getRequestUserId
  settings <- requireCheckJsonBody
  runDB do
    update \u -> do
      set u [UserBeta =. val (betaSetting settings)]
      where_ $ u.id ==. val userId
    User {..} <- get404 userId
    pure $ CurrentUser userUsername userEmail userBeta
