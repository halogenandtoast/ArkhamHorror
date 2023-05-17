module Arkham.Story.Cards.EngramsOath (
  EngramsOath (..),
  engramsOath,
) where

import Arkham.Prelude

import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype EngramsOath = EngramsOath StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

engramsOath :: StoryCard EngramsOath
engramsOath = story EngramsOath Cards.engramsOath

instance RunMessage EngramsOath where
  runMessage msg s@(EngramsOath attrs) = case msg of
    ResolveStory _ story' | story' == toId attrs -> do
      push $ Remember InterviewedConstance
      pure s
    _ -> EngramsOath <$> runMessage msg attrs
