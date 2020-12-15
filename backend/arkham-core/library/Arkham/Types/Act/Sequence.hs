module Arkham.Types.Act.Sequence where

import Arkham.Prelude

import Arkham.Types.ActId

actStep :: ActSequence -> ActStep
actStep (Act num _) = ActStep num

data ActSide = A | B
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActSequence = Act Int ActSide
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
