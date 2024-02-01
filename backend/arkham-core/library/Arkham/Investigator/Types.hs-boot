module Arkham.Investigator.Types where

import Arkham.Prelude

data Investigator

instance NoThunks Investigator
instance Show Investigator
instance Eq Investigator
instance ToJSON Investigator
