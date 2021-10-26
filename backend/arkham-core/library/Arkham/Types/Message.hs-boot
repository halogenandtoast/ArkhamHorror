module Arkham.Types.Message where

import Arkham.Prelude

data Message

instance ToJSON Message
instance FromJSON Message
instance Show Message
instance Eq Message
