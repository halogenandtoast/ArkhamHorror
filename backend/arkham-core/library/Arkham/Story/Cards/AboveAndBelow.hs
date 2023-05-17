module Arkham.Story.Cards.AboveAndBelow (
  AboveAndBelow (..),
  aboveAndBelow,
) where

import Arkham.Prelude

import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype AboveAndBelow = AboveAndBelow StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aboveAndBelow :: StoryCard AboveAndBelow
aboveAndBelow = story AboveAndBelow Cards.aboveAndBelow

instance RunMessage AboveAndBelow where
  runMessage msg s@(AboveAndBelow attrs) = case msg of
    ResolveStory _ story' | story' == toId attrs -> do
      push $ Remember InterviewedAshleigh
      pure s
    _ -> AboveAndBelow <$> runMessage msg attrs
