{-# LANGUAGE TemplateHaskell #-}

module Arkham.Act.Sequence where

import Arkham.Prelude
import Data.Aeson.TH

newtype ActStep = ActStep {unActStep :: Int}
  deriving stock (Data)
  deriving newtype (Eq, NoThunks, NFData)

actStep :: ActSequence -> ActStep
actStep (Sequence num _) = ActStep num

actSide :: ActSequence -> ActSide
actSide (Sequence _ side) = side

data ActSide = A | B | C | D | E | F
  deriving stock (Eq, Show, Ord, Data, Generic)
  deriving anyclass (NoThunks, NFData)

data ActSequence = Sequence Int ActSide
  deriving stock (Eq, Show, Ord, Data, Generic)
  deriving anyclass (NoThunks, NFData)

$(deriveJSON defaultOptions ''ActSide)
$(deriveJSON defaultOptions ''ActSequence)
