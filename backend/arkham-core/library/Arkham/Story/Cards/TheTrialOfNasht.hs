module Arkham.Story.Cards.TheTrialOfNasht (
  TheTrialOfNasht (..),
  theTrialOfNasht,
) where

import Arkham.Prelude

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheTrialOfNasht = TheTrialOfNasht StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theTrialOfNasht :: StoryCard TheTrialOfNasht
theTrialOfNasht = story TheTrialOfNasht Cards.theTrialOfNasht

instance RunMessage TheTrialOfNasht where
  runMessage msg s@(TheTrialOfNasht attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      push $ AddToVictory (toTarget attrs)
      pure s
    _ -> TheTrialOfNasht <$> runMessage msg attrs
