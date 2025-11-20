module Arkham.Story.Cards.TheTrialOfNasht (theTrialOfNasht) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheTrialOfNasht = TheTrialOfNasht StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrialOfNasht :: StoryCard TheTrialOfNasht
theTrialOfNasht = story TheTrialOfNasht Cards.theTrialOfNasht

instance RunMessage TheTrialOfNasht where
  runMessage msg s@(TheTrialOfNasht attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      addToVictory iid attrs
      pure s
    _ -> TheTrialOfNasht <$> liftRunMessage msg attrs
