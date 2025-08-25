{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Base.Api.Handler.CurrentUser where

import Import

data CurrentUser = CurrentUser
  { username :: Text
  , email :: Text
  , beta :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

getApiV1CurrentUserR :: Handler CurrentUser
getApiV1CurrentUserR = do
  userId <- getRequestUserId
  runDB do
    User {..} <- get404 userId
    pure $ CurrentUser userUsername userEmail userBeta
