{-# LANGUAGE TemplateHaskell #-}

module Arkham.Card.Cost (CardCost (..), toPrintedCost) where

import Arkham.Calculation
import Arkham.Prelude
import Data.Aeson.TH

toPrintedCost :: CardCost -> Int
toPrintedCost (StaticCost n) = n
toPrintedCost DynamicCost = 0
toPrintedCost (MaxDynamicCost _) = 0
toPrintedCost DiscardAmountCost = 0

data CardCost = StaticCost Int | DynamicCost | DiscardAmountCost | MaxDynamicCost GameCalculation
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''CardCost)
