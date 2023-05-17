module Arkham.Story.Cards.UnfinishedBusiness_L
  ( UnfinishedBusiness_L(..)
  , unfinishedBusiness_L
  ) where

import Arkham.Prelude

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype UnfinishedBusiness_L = UnfinishedBusiness_L StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_L :: StoryCard UnfinishedBusiness_L
unfinishedBusiness_L = story UnfinishedBusiness_L Cards.unfinishedBusiness_L

instance RunMessage UnfinishedBusiness_L where
  runMessage msg s@(UnfinishedBusiness_L attrs) = case msg of
    ResolveStory _ story' | story' == toId attrs -> do
      pure s
    _ -> UnfinishedBusiness_L <$> runMessage msg attrs
