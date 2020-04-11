{-# LANGUAGE OverloadedStrings  #-}

module Handler.Api.Authentication where

import           Data.Text
import           Import

newtype Token = Token { token :: Text }
  deriving stock (Generic)

instance ToJSON Token

postApiV1AuthenticationR :: Handler Token
postApiV1AuthenticationR = runDB $ do
  Entity userId _ <- getBy404 $ UniqueEmail "halogenandtoast@gmail.com"
  lift $ Token <$> userIdToToken userId
