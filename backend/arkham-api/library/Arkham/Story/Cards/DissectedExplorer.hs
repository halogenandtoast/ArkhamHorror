module Arkham.Story.Cards.DissectedExplorer
  ( DissectedExplorer(..)
  , dissectedExplorer
  ) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DissectedExplorer = DissectedExplorer StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dissectedExplorer :: StoryCard DissectedExplorer
dissectedExplorer = story DissectedExplorer Cards.dissectedExplorer

instance RunMessage DissectedExplorer where
  runMessage msg s@(DissectedExplorer attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> DissectedExplorer <$> liftRunMessage msg attrs
