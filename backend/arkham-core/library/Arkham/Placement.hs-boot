module Arkham.Placement where

import Arkham.Prelude

data Placement

instance NoThunks Placement
instance NFData Placement
instance Data Placement
instance Show Placement
instance Eq Placement
instance ToJSON Placement
instance FromJSON Placement
instance Ord Placement
