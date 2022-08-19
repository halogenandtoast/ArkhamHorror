{-# LANGUAGE DerivingVia #-}
module Types where

import Data.Text
import GHC.Generics
import Json
import Prelude ( Maybe (Just), ($) )

newtype Token = Token { getToken :: Text }

instance ToJSON Token where
  toJSON token = object ["token" .= getToken token]

data Registration = Registration
  { registrationEmail :: Text
  , registrationUsername :: Text
  , registrationPassword :: Text
  }
  deriving stock Generic

instance ToJSON Registration where
  toJSON = genericToJSON $ aesonOptions $ Just "registration"
  toEncoding = genericToEncoding $ aesonOptions $ Just "registration"

instance FromJSON Registration where
  parseJSON = genericParseJSON $ aesonOptions $ Just "registration"

data Authentication = Authentication
  { authenticationEmail :: Text
  , authenticationPassword :: Text
  }
  deriving stock Generic

instance FromJSON Authentication where
  parseJSON = genericParseJSON $ aesonOptions $ Just "authentication"
