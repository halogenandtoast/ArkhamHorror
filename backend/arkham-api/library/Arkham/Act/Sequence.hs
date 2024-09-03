{-# LANGUAGE TemplateHaskell #-}

module Arkham.Act.Sequence where

import Arkham.Prelude
import Data.Aeson.TH

newtype ActStep = ActStep {unActStep :: Int}
  deriving stock (Data)
  deriving newtype (Eq)

actStep :: ActSequence -> ActStep
actStep (Sequence num _) = ActStep num

actSide :: ActSequence -> ActSide
actSide (Sequence _ side) = side

data ActSide = A | B | C | D | E | F
  deriving stock (Eq, Show, Ord, Data)

data ActSequence = Sequence Int ActSide
  deriving stock (Eq, Show, Ord, Data)

$(deriveJSON defaultOptions ''ActSide)
$(deriveJSON defaultOptions ''ActSequence)
