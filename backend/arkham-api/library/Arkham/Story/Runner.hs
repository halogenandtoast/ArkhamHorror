{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Story.Runner (
  module Arkham.Story.Runner,
  module X,
) where

import Arkham.Prelude

import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.Helpers.Effect as X
import Arkham.Helpers.Message as X hiding (story)
import Arkham.Helpers.Query as X
import Arkham.Id as X
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Story.Types as X
import Arkham.Target as X

import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario
import Arkham.Scenario.Types (Field (..))
import Control.Lens (non)

afterStoryResolution :: HasQueue Message m => StoryAttrs -> [Message] -> m ()
afterStoryResolution (toId -> storyId) = traverse_ (pushAfter isResolution) . reverse
 where
  isResolution = \case
    ResolvedStory _ story' | story' == storyId -> True
    _ -> False

getAlreadyResolved :: HasGame m => StoryAttrs -> m Bool
getAlreadyResolved (toId -> storyId) = scenarioFieldMap ScenarioResolvedStories (elem storyId)

instance RunMessage Story where
  runMessage msg (Story a) = Story <$> runMessage msg a

instance RunMessage StoryAttrs where
  runMessage msg attrs = case msg of
    ResolvedStory _ story' | story' == toId attrs -> do
      pushWhen (storyRemoveAfterResolution attrs) $ RemoveStory (toId attrs)
      pure attrs
    ResolveStory iid DoNotResolveIt story' | story' == toId attrs -> do
      player <- getPlayer iid
      push $ chooseOne player [targetLabel (toTarget attrs) []]
      pure attrs
    PlaceUnderneath target cards | isTarget attrs target -> do
      pure $ attrs & cardsUnderneathL <>~ cards
    UseAbility _ ab _ | isSource attrs ab.source || isProxySource attrs ab.source -> do
      push $ Do msg
      pure attrs
    InSearch msg'@(UseAbility _ ab _) | isSource attrs ab.source || isProxySource attrs ab.source -> do
      push $ Do msg'
      pure attrs
    InDiscard _ msg'@(UseAbility _ ab _) | isSource attrs ab.source || isProxySource attrs ab.source -> do
      push $ Do msg'
      pure attrs
    InHand _ msg'@(UseAbility _ ab _) | isSource attrs ab.source || isProxySource attrs ab.source -> do
      push $ Do msg'
      pure attrs
    PlaceTokens _source target tType n | isTarget attrs target -> do
      pure $ attrs & tokensL . at tType . non 0 %~ (+ n)
    _ -> pure attrs
