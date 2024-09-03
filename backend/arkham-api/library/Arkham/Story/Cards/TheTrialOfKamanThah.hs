module Arkham.Story.Cards.TheTrialOfKamanThah (
  TheTrialOfKamanThah (..),
  theTrialOfKamanThah,
) where

import Arkham.Prelude

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheTrialOfKamanThah = TheTrialOfKamanThah StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrialOfKamanThah :: StoryCard TheTrialOfKamanThah
theTrialOfKamanThah = story TheTrialOfKamanThah Cards.theTrialOfKamanThah

instance RunMessage TheTrialOfKamanThah where
  runMessage msg s@(TheTrialOfKamanThah attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      push $ AddToVictory (toTarget attrs)
      pure s
    _ -> TheTrialOfKamanThah <$> runMessage msg attrs
