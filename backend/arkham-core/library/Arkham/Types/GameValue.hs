module Arkham.Types.GameValue
  ( GameValue(..)
  , fromGameValue
  ) where

import Arkham.Prelude

data GameValue a
  = Static a
  | PerPlayer a
  | StaticWithPerPlayer a a
  | ByPlayerCount a a a a
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Functor GameValue where
  fmap f (Static n) = Static (f n)
  fmap f (PerPlayer n) = PerPlayer (f n)
  fmap f (StaticWithPerPlayer n m) = StaticWithPerPlayer (f n) (f m)
  fmap f (ByPlayerCount n1 n2 n3 n4) =
    ByPlayerCount (f n1) (f n2) (f n3) (f n4)

fromGameValue :: GameValue Int -> Int -> Int
fromGameValue (Static n) _ = n
fromGameValue (PerPlayer n) pc = n * pc
fromGameValue (StaticWithPerPlayer n m) pc = n + (m * pc)
fromGameValue (ByPlayerCount n1 n2 n3 n4) pc = case pc of
  1 -> n1
  2 -> n2
  3 -> n3
  4 -> n4
  _ -> error "Unhandled by player count value"
