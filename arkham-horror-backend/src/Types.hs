{-# LANGUAGE DerivingVia #-}
module Types where

import Data.Aeson
import GHC.Generics
import Data.Text
import Prelude (($), Maybe(Just))
import Utility

newtype Token = Token { getToken :: Text }

instance ToJSON Token where
  toJSON token = object [ "token" .= getToken token ]

data Registration = Registration
    { registrationEmail    :: Text
    , registrationUsername :: Text
    , registrationPassword :: Text
    }
    deriving stock (Generic)
    deriving ToJSON via Codec (Drop "registration") Registration

instance FromJSON Registration where
  parseJSON = genericParseJSON $ aesonOptions $ Just "registration"

data Authentication = Authentication
    { authenticationEmail    :: Text
    , authenticationPassword :: Text
    }
    deriving stock (Generic)

instance FromJSON Authentication where
  parseJSON = genericParseJSON $ aesonOptions $ Just "authentication"
