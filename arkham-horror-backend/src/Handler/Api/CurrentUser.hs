{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Api.CurrentUser where

import Import

newtype CurrentUser = CurrentUser { username :: Text }
  deriving stock (Generic)

instance ToJSON CurrentUser

getApiV1CurrentUserR :: Handler CurrentUser
getApiV1CurrentUserR = pure $ CurrentUser "halogen64"
