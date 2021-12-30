module Arkham.Window
  ( Window
  ) where

import Arkham.Prelude

data Window

instance Eq Window
instance Show Window
instance ToJSON Window
instance FromJSON Window
instance Hashable Window
