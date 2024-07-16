module Arkham.Message where

import Arkham.Prelude

data Message

instance Data Message
instance Show Message
instance Eq Message
instance ToJSON Message
instance FromJSON Message
