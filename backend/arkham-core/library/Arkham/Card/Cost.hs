{-# LANGUAGE TemplateHaskell #-}

module Arkham.Card.Cost (
  CardCost (..),
  toPrintedCost,
) where

import Arkham.Prelude

import Data.Aeson.TH

toPrintedCost :: CardCost -> Int
toPrintedCost (StaticCost n) = n
toPrintedCost DynamicCost = 0

data CardCost = StaticCost Int | DynamicCost
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''CardCost)
