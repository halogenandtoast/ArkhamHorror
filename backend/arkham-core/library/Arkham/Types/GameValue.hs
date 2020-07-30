module Arkham.Types.GameValue
  ( GameValue(..)
  , fromGameValue
  )
where

import ClassyPrelude
import Data.Aeson

data GameValue
  = Static Int
  | PerPlayer Int
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

fromGameValue :: GameValue -> Int -> Int
fromGameValue (Static n) _ = n
fromGameValue (PerPlayer n) pc = n * pc
