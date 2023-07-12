module Arkham.Story.Cards.JeromesFate (
  JeromesFate (..),
  jeromesFate,
) where

import Arkham.Prelude

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype JeromesFate = JeromesFate StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeromesFate :: StoryCard JeromesFate
jeromesFate = story JeromesFate Cards.jeromesFate

instance RunMessage JeromesFate where
  runMessage msg s@(JeromesFate attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> JeromesFate <$> runMessage msg attrs
