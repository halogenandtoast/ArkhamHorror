module Arkham.Difficulty where

import Arkham.Prelude

data Difficulty
  = Easy
  | Standard
  | Hard
  | Expert
  deriving stock (Eq, Show, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

fromDifficulty :: a -> a -> a -> a -> Difficulty -> a
fromDifficulty easy standard hard expert = \case
  Easy -> easy
  Standard -> standard
  Hard -> hard
  Expert -> expert
