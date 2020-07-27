module Arkham.Types.Card.Class where

import Arkham.Types.Card.CardCode
import ClassyPrelude

class HasCardCode a where
  getCardCode :: a -> CardCode

class HasCost a where
  getCost :: a -> Int
