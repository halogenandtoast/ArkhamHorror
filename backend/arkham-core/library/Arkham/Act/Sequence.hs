module Arkham.Act.Sequence where

import Arkham.Prelude

newtype ActStep = ActStep { unActStep :: Int }
  deriving newtype Eq

actStep :: ActSequence -> ActStep
actStep (Sequence num _) = ActStep num

actSide :: ActSequence -> ActSide
actSide (Sequence _ side) = side

data ActSide = A | B | C | D | E | F
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data ActSequence = Sequence Int ActSide
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
