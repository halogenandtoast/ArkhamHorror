{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Story where

import Arkham.Card
import Arkham.Id
import Arkham.Placement
import Arkham.Prelude
import Arkham.Target
import Data.Aeson.TH

data StoryMode = ResolveIt | DoNotResolveIt
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data StoryMessage
  = ReadStory InvestigatorId Card StoryMode (Maybe Target)
  | ReadStoryWithPlacement InvestigatorId Card StoryMode (Maybe Target) Placement
  | ResolveStory InvestigatorId StoryMode StoryId
  | -- | The resolver, as 'ResolveStory' carries it, so the resolution window can name a "you".
    ResolvedStory InvestigatorId StoryMode StoryId
  | PlaceStory Card Placement
  | RemoveStory StoryId
  deriving stock (Show, Ord, Eq, Data, Generic)

$(deriveToJSON defaultOptions ''StoryMessage)

instance FromJSON StoryMessage where
  parseJSON = withObject "StoryMessage" \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      {- Used to carry only the mode and the story. The resolving investigator was
      added so the story resolution window can name a "you"; nothing could have
      been watching that window before it existed, so a message queued by an older
      build decodes to the placeholder id and its window matches no one. -}
      "ResolvedStory" -> do
        contents <- (Right <$> o .: "contents") <|> (Left <$> o .: "contents")
        case contents of
          Right (a, b, c) -> pure $ ResolvedStory a b c
          Left (b, c) -> pure $ ResolvedStory (InvestigatorId "00000") b c
      _ -> genericParseJSON defaultOptions (Object o)
