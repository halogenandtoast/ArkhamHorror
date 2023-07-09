module Arkham.Location.Brazier where

import Arkham.Prelude

data Brazier = Lit | Unlit
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
