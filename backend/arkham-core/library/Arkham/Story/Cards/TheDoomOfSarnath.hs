module Arkham.Story.Cards.TheDoomOfSarnath (TheDoomOfSarnath (..), theDoomOfSarnath) where

import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheDoomOfSarnath = TheDoomOfSarnath StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDoomOfSarnath :: StoryCard TheDoomOfSarnath
theDoomOfSarnath = story TheDoomOfSarnath Cards.theDoomOfSarnath

instance RunMessage TheDoomOfSarnath where
  runMessage msg s@(TheDoomOfSarnath attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      push $ ScenarioCountIncrementBy SignOfTheGods 1
      pure s
    _ -> TheDoomOfSarnath <$> runMessage msg attrs
