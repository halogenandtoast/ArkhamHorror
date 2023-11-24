module Arkham.Story.Cards.TheInfestationBegins
  ( TheInfestationBegins(..)
  , theInfestationBegins
  ) where

import Arkham.Prelude

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheInfestationBegins = TheInfestationBegins StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theInfestationBegins :: StoryCard TheInfestationBegins
theInfestationBegins = story TheInfestationBegins Cards.theInfestationBegins

instance RunMessage TheInfestationBegins where
  runMessage msg s@(TheInfestationBegins attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> TheInfestationBegins <$> runMessage msg attrs
