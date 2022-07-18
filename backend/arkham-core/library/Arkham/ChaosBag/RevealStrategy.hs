module Arkham.ChaosBag.RevealStrategy where

import Arkham.Prelude

data RevealStrategy
  = Reveal Int
  | RevealAndChoose Int Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
