{-# LANGUAGE TemplateHaskell #-}

module Arkham.Card.Cost (CardCost (..), toPrintedCost) where

import Arkham.Calculation
import Arkham.Matcher.Card
import Arkham.Prelude
import Data.Aeson.TH

toPrintedCost :: CardCost -> Int
toPrintedCost (StaticCost n) = n
toPrintedCost DynamicCost = 0
toPrintedCost (MaxDynamicCost _) = 0
toPrintedCost DiscardAmountCost = 0
toPrintedCost AnyMatchingCardCost{} = 0

data CardCost
  = StaticCost Int
  | DynamicCost
  | DiscardAmountCost
  | MaxDynamicCost GameCalculation
  | AnyMatchingCardCost ExtendedCardMatcher
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''CardCost)
