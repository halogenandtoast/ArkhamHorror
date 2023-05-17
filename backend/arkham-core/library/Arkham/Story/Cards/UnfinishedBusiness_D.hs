module Arkham.Story.Cards.UnfinishedBusiness_D
  ( UnfinishedBusiness_D(..)
  , unfinishedBusiness_D
  ) where

import Arkham.Prelude

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype UnfinishedBusiness_D = UnfinishedBusiness_D StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_D :: StoryCard UnfinishedBusiness_D
unfinishedBusiness_D = story UnfinishedBusiness_D Cards.unfinishedBusiness_D

instance RunMessage UnfinishedBusiness_D where
  runMessage msg s@(UnfinishedBusiness_D attrs) = case msg of
    ResolveStory _ story' | story' == toId attrs -> do
      pure s
    _ -> UnfinishedBusiness_D <$> runMessage msg attrs
