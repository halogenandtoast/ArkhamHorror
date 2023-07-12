module Arkham.Story.Cards.ValentinosFate (
  ValentinosFate (..),
  valentinosFate,
) where

import Arkham.Prelude

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype ValentinosFate = ValentinosFate StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valentinosFate :: StoryCard ValentinosFate
valentinosFate = story ValentinosFate Cards.valentinosFate

instance RunMessage ValentinosFate where
  runMessage msg s@(ValentinosFate attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> ValentinosFate <$> runMessage msg attrs
