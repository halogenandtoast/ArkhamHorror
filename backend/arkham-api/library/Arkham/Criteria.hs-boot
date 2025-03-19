module Arkham.Criteria where

import Arkham.Prelude
import Arkham.Matcher.Base

data Criterion

instance Data Criterion
instance Show Criterion
instance Eq Criterion
instance Ord Criterion
instance ToJSON Criterion
instance FromJSON Criterion
instance OneOf Criterion
