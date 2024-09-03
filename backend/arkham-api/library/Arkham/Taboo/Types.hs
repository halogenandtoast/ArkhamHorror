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
  _ -> Nothing

displayTabooList :: TabooList -> Text
displayTabooList = \case
  TabooList15 -> "1.5 (Apr 23, 2019)"
  TabooList16 -> "1.6 (Sep 27, 2019)"
  TabooList18 -> "1.8 (Oct 15, 2020)"
  TabooList19 -> "1.9 (Jun 28, 2021)"
  TabooList20 -> "2.0 (Aug 26, 2022)"
  TabooList21 -> "2.1 (Aug 30, 2023)"
  TabooList22 -> "2.2 (Feb 20, 2024)"

$(deriveJSON (defaultOptions {tagSingleConstructors = True}) ''TabooList)
