module Arkham.Card.Class where

import ClassyPrelude

class HasCost a where
  getCost :: a -> Int
