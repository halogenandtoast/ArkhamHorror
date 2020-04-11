{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Handler.Api.CurrentUser where

import           Import

newtype CurrentUser = CurrentUser { username :: Text }
  deriving stock (Generic)

instance ToJSON CurrentUser

getApiV1CurrentUserR :: Handler CurrentUser
getApiV1CurrentUserR = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> notAuthenticated
    Just userId -> runDB $ do
      User {..} <- get404 userId
      pure $ CurrentUser userUsername
