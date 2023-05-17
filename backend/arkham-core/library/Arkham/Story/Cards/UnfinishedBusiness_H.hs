module Arkham.Story.Cards.UnfinishedBusiness_H
  ( UnfinishedBusiness_H(..)
  , unfinishedBusiness_H
  ) where

import Arkham.Prelude

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype UnfinishedBusiness_H = UnfinishedBusiness_H StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_H :: StoryCard UnfinishedBusiness_H
unfinishedBusiness_H = story UnfinishedBusiness_H Cards.unfinishedBusiness_H

instance RunMessage UnfinishedBusiness_H where
  runMessage msg s@(UnfinishedBusiness_H attrs) = case msg of
    ResolveStory _ story' | story' == toId attrs -> do
      pure s
    _ -> UnfinishedBusiness_H <$> runMessage msg attrs
