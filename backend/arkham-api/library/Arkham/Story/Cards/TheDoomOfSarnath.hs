module Arkham.Story.Cards.TheDoomOfSarnath (theDoomOfSarnath) where

import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheDoomOfSarnath = TheDoomOfSarnath StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDoomOfSarnath :: StoryCard TheDoomOfSarnath
theDoomOfSarnath = story TheDoomOfSarnath Cards.theDoomOfSarnath

instance RunMessage TheDoomOfSarnath where
  runMessage msg s@(TheDoomOfSarnath attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      push $ ScenarioCountIncrementBy SignOfTheGods 1
      pure s
    _ -> TheDoomOfSarnath <$> liftRunMessage msg attrs
