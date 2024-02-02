module Arkham.Difficulty where

import Arkham.Prelude

data Difficulty
  = Easy
  | Standard
  | Hard
  | Expert
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)

fromDifficulty :: a -> a -> a -> a -> Difficulty -> a
fromDifficulty easy standard hard expert = \case
  Easy -> easy
  Standard -> standard
  Hard -> hard
  Expert -> expert
