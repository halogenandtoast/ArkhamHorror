module Arkham.Act.Sequence where

import Arkham.Prelude

newtype ActStep = ActStep { unActStep :: Int }
  deriving newtype Eq

actStep :: ActSequence -> ActStep
actStep (Act num _) = ActStep num

actSide :: ActSequence -> ActSide
actSide (Act _ side) = side

data ActSide = A | B
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActSequence = Act Int ActSide
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
