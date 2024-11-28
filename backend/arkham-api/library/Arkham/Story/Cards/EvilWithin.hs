module Arkham.Story.Cards.EvilWithin
  ( EvilWithin(..)
  , evilWithin
  ) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype EvilWithin = EvilWithin StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evilWithin :: StoryCard EvilWithin
evilWithin = story EvilWithin Cards.evilWithin

instance RunMessage EvilWithin where
  runMessage msg s@(EvilWithin attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> EvilWithin <$> liftRunMessage msg attrs
