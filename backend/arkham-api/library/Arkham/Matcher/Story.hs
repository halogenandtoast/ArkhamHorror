{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Story where

import Arkham.Card.CardCode
import {-# SOURCE #-} Arkham.Placement
import Arkham.Prelude
import Data.Aeson.TH

data StoryMatcher
  = StoryWithTitle Text
  | StoryWithPlacement Placement
  | StoryMatchAll [StoryMatcher]
  | StoryIs CardCode
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup StoryMatcher where
  StoryMatchAll xs <> StoryMatchAll ys = StoryMatchAll (xs <> ys)
  StoryMatchAll xs <> x = StoryMatchAll $ xs <> [x]
  x <> StoryMatchAll xs = StoryMatchAll (x : xs)
  x <> y = StoryMatchAll [x, y]

$(deriveJSON defaultOptions ''StoryMatcher)
