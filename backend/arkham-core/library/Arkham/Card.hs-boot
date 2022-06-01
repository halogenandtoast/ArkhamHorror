module Arkham.Card where

import Arkham.Prelude

data Card

instance Show Card
instance Eq Card
instance Hashable Card
instance ToJSON Card
instance FromJSON Card
