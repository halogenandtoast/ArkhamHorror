module Arkham.Cost where

import Arkham.Prelude

data Cost

instance NoThunks Cost
instance NFData Cost
instance Data Cost
instance Show Cost
instance Eq Cost
instance Ord Cost
instance ToJSON Cost
instance FromJSON Cost
