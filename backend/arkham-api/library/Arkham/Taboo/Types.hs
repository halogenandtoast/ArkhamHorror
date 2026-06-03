{-# LANGUAGE TemplateHaskell #-}

module Arkham.Taboo.Types where

import Arkham.Prelude
import Data.Aeson.TH

data TabooList
  = TabooList15
  | TabooList16
  | TabooList18
  | TabooList19
  | TabooList20
  | TabooList21
  | TabooList22
  | TabooList23
  | TabooList24
  | TabooList25
  deriving stock (Show, Eq, Ord, Data)

fromTabooId :: Maybe Int -> Maybe TabooList
fromTabooId = \case
  Just 1 -> Just TabooList15
  Just 2 -> Just TabooList16
  Just 3 -> Just TabooList18
  Just 4 -> Just TabooList19
  Just 5 -> Just TabooList20
  Just 6 -> Just TabooList21
  Just 7 -> Just TabooList22
  Just 8 -> Just TabooList23
  Just 9 -> Just TabooList24
  Just 10 -> Just TabooList25
  _ -> Nothing
$(deriveJSON (defaultOptions {tagSingleConstructors = True}) ''TabooList)
