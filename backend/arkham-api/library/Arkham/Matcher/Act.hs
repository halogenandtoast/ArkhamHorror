{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Act where

import Arkham.Act.Sequence
import Arkham.Id
import Arkham.Matcher.Treachery
import Arkham.Matcher.Value
import {-# SOURCE #-} Arkham.Modifier
import Arkham.Prelude
import Arkham.Token
import Data.Aeson.TH

data ActMatcher
  = ActWithId ActId
  | AnyAct
  | ActWithSide ActSide
  | ActWithStep Int
  | ActWithTreachery TreacheryMatcher
  | ActWithDeckId Int
  | ActWithModifier ModifierType
  | ActWithTokens Token ValueMatcher
  | NotAct ActMatcher
  | ActOneOf [ActMatcher]
  | ActCanWheelOfFortuneX
  deriving stock (Show, Eq, Ord, Data)
newtype RemainingActMatcher = RemainingActMatcher {unRemainingActMatcher :: ActMatcher}
  deriving stock (Show, Eq, Ord)

pattern ActWithClues :: ValueMatcher -> ActMatcher
pattern ActWithClues n <- ActWithTokens Clue n
  where
    ActWithClues n = ActWithTokens Clue n

$(deriveJSON defaultOptions ''ActMatcher)
