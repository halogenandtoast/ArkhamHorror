{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Story (
  Story (..),
  createStory,
  lookupStory,
) where

import Arkham.Prelude hiding (fold)

import Arkham.Card
import Arkham.Homebrew.Registry qualified as Registry
import Arkham.Id
import Arkham.Story.Stories
import Arkham.Story.Types
import Arkham.Target

createStory :: IsCard a => a -> Maybe Target -> StoryId -> Story
createStory a mtarget sId = lookupStory sId mtarget (toCardId a)

lookupStory :: StoryId -> Maybe Target -> CardId -> Story
lookupStory storyId = case lookup (unStoryId storyId) allStories of
  Nothing -> error $ "Unknown story: " <> show storyId
  Just (SomeStoryCard a) -> \mtarget cardId -> Story $ cbCardBuilder a cardId (mtarget, storyId)

instance FromJSON Story where
  parseJSON = withObject "Story" $ \o -> do
    cCode <- o .: "id"
    withStoryCardCode cCode $ \(_ :: StoryCard a) -> Story <$> parseJSON @a (Object o)

withStoryCardCode :: CardCode -> (forall a. IsStory a => StoryCard a -> r) -> r
withStoryCardCode cCode f = case lookup cCode allStories of
  Nothing -> error $ "Unknown story: " <> show cCode
  Just (SomeStoryCard a) -> f a

allStories :: Map CardCode SomeStoryCard
allStories =
  (mapFrom someStoryCardCode Registry.stories <>)
    $ mapFrom someStoryCardCode allStoryCardBuilders
