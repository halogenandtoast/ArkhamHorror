module Arkham.Story.Cards.ThePattern (
  ThePattern (..),
  thePattern,
) where

import Arkham.Prelude

import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype ThePattern = ThePattern StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePattern :: StoryCard ThePattern
thePattern = story ThePattern Cards.thePattern

instance RunMessage ThePattern where
  runMessage msg s@(ThePattern attrs) = case msg of
    ResolveStory _ _ story' | story' == toId attrs -> do
      push $ Remember InterviewedHaruko
      pure s
    _ -> ThePattern <$> runMessage msg attrs
