module Arkham.Story.Cards.UnfinishedBusiness_B
  ( UnfinishedBusiness_B(..)
  , unfinishedBusiness_B
  ) where

import Arkham.Prelude

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype UnfinishedBusiness_B = UnfinishedBusiness_B StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_B :: StoryCard UnfinishedBusiness_B
unfinishedBusiness_B = story UnfinishedBusiness_B Cards.unfinishedBusiness_B

instance RunMessage UnfinishedBusiness_B where
  runMessage msg s@(UnfinishedBusiness_B attrs) = case msg of
    ResolveStory _ story' | story' == toId attrs -> do
      pure s
    _ -> UnfinishedBusiness_B <$> runMessage msg attrs
