module Arkham.Criteria where

import Arkham.Prelude

data Criterion

instance NoThunks Criterion
instance Data Criterion
instance Show Criterion
instance Eq Criterion
instance Ord Criterion
instance ToJSON Criterion
instance FromJSON Criterion
