module Arkham.Story.Cards.GavriellasFate (
  GavriellasFate (..),
  gavriellasFate,
) where

import Arkham.Prelude

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype GavriellasFate = GavriellasFate StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gavriellasFate :: StoryCard GavriellasFate
gavriellasFate = story GavriellasFate Cards.gavriellasFate

instance RunMessage GavriellasFate where
  runMessage msg s@(GavriellasFate attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> GavriellasFate <$> runMessage msg attrs
