module Arkham.History where

import Arkham.Prelude

data History

instance NoThunks History
instance Show History
instance Eq History
instance ToJSON History
instance FromJSON History
