module Arkham.Card.Class where

import Arkham.Prelude

class HasCost a where
  getCost :: a -> Int
