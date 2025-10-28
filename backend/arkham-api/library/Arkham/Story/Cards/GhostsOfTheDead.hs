module Arkham.Story.Cards.GhostsOfTheDead (ghostsOfTheDead) where

import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype GhostsOfTheDead = GhostsOfTheDead StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghostsOfTheDead :: StoryCard GhostsOfTheDead
ghostsOfTheDead = story GhostsOfTheDead Cards.ghostsOfTheDead

instance RunMessage GhostsOfTheDead where
  runMessage msg s@(GhostsOfTheDead attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      remember KnowWhatHappenedToIb
      pure s
    _ -> GhostsOfTheDead <$> liftRunMessage msg attrs
