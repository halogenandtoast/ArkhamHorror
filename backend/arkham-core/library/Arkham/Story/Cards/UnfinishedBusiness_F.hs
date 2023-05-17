module Arkham.Story.Cards.UnfinishedBusiness_F
  ( UnfinishedBusiness_F(..)
  , unfinishedBusiness_F
  ) where

import Arkham.Prelude

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype UnfinishedBusiness_F = UnfinishedBusiness_F StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_F :: StoryCard UnfinishedBusiness_F
unfinishedBusiness_F = story UnfinishedBusiness_F Cards.unfinishedBusiness_F

instance RunMessage UnfinishedBusiness_F where
  runMessage msg s@(UnfinishedBusiness_F attrs) = case msg of
    ResolveStory _ story' | story' == toId attrs -> do
      pure s
    _ -> UnfinishedBusiness_F <$> runMessage msg attrs
