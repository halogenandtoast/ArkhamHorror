{-# LANGUAGE TemplateHaskell #-}

module Arkham.Card.Cost (CardCost (..), EnemyCostField (..), toPrintedCost) where

import Arkham.Calculation
import Arkham.Matcher.Card
import Arkham.Matcher.Enemy (EnemyMatcher)
import Arkham.Prelude
import Data.Aeson.TH

toPrintedCost :: CardCost -> Int
toPrintedCost (StaticCost n) = n
toPrintedCost DynamicCost = 0
toPrintedCost (MaxDynamicCost _) = 0
toPrintedCost DiscardAmountCost = 0
toPrintedCost AnyMatchingCardCost{} = 0
toPrintedCost MatchingEnemyFieldCost{} = 0
toPrintedCost DeferredCost = 0

data EnemyCostField
  = EnemyRemainingHealthField
  deriving stock (Show, Eq, Ord, Data)

data CardCost
  = StaticCost Int
  | DynamicCost
  | DiscardAmountCost
  | MaxDynamicCost GameCalculation
  | AnyMatchingCardCost ExtendedCardMatcher
  | MatchingEnemyFieldCost EnemyMatcher EnemyCostField
  | DeferredCost
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''EnemyCostField)
$(deriveJSON defaultOptions ''CardCost)
