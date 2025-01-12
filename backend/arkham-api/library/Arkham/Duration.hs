module Arkham.Duration where

class Duration a where
  type DurationOf a
  during :: DurationOf a -> a
