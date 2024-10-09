{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Act where

import Arkham.Act.Sequence
import Arkham.Id
import Arkham.Matcher.Treachery
import Arkham.Prelude
import Data.Aeson.TH

data ActMatcher
  = ActWithId ActId
  | AnyAct
  | ActWithSide ActSide
  | ActWithTreachery TreacheryMatcher
  | ActWithDeckId Int
  | NotAct ActMatcher
  | ActOneOf [ActMatcher]
  | ActCanWheelOfFortuneX
  deriving stock (Show, Eq, Ord, Data)

newtype RemainingActMatcher = RemainingActMatcher {unRemainingActMatcher :: ActMatcher}
  deriving stock (Show, Eq, Ord)

$(deriveJSON defaultOptions ''ActMatcher)
