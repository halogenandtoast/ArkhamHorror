module Arkham.Story.Cards.PennysFate (
  PennysFate (..),
  pennysFate,
) where

import Arkham.Prelude

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype PennysFate = PennysFate StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pennysFate :: StoryCard PennysFate
pennysFate = story PennysFate Cards.pennysFate

instance RunMessage PennysFate where
  runMessage msg s@(PennysFate attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> PennysFate <$> runMessage msg attrs
