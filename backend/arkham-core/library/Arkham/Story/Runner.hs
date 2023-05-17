{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Story.Runner (
  module X,
) where

import Arkham.Prelude

import Arkham.Classes as X
import Arkham.Helpers.Message as X
import Arkham.Message as X hiding (story)
import Arkham.Story.Types as X
import Arkham.Target as X

import Arkham.Placement

instance RunMessage Story where
  runMessage msg (Story a) = Story <$> runMessage msg a

instance RunMessage StoryAttrs where
  runMessage msg attrs = case msg of
    ResolvedStory story' | story' == toId attrs -> do
      when (storyPlacement attrs == Unplaced) $
        push $
          RemoveStory $
            toId attrs
      pure attrs
    _ -> pure attrs
