{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Story where

import Arkham.Card
import Arkham.Id
import Arkham.Placement
import Arkham.Prelude
import Arkham.Target
import Data.Aeson.TH

data StoryMode = ResolveIt | DoNotResolveIt
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data StoryMessage
  = ReadStory InvestigatorId Card StoryMode (Maybe Target)
  | ReadStoryWithPlacement InvestigatorId Card StoryMode (Maybe Target) Placement
  | ResolveStory InvestigatorId StoryMode StoryId
  | ResolvedStory StoryMode StoryId
  | PlaceStory Card Placement
  | RemoveStory StoryId
  deriving stock (Show, Eq, Data)

$(deriveJSON defaultOptions ''StoryMessage)
