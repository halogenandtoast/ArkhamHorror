{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Handler.Api.Authentication where

import Data.Text
import Import

newtype Token = Token { token :: Text }
  deriving stock (Generic)

instance ToJSON Token

postApiV1AuthenticationR :: Handler Token
postApiV1AuthenticationR = pure $ Token "abc123"
