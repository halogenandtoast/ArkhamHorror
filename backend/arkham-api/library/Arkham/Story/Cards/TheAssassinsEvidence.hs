module Arkham.Story.Cards.TheAssassinsEvidence (theAssassinsEvidence) where

import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheAssassinsEvidence = TheAssassinsEvidence StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAssassinsEvidence :: StoryCard TheAssassinsEvidence
theAssassinsEvidence = story TheAssassinsEvidence Cards.theAssassinsEvidence & persistStory

instance RunMessage TheAssassinsEvidence where
  runMessage msg s@(TheAssassinsEvidence attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      resolveEvidence attrs "theAssassinsEvidence.benefit"
      pure s
    _ -> TheAssassinsEvidence <$> liftRunMessage msg attrs
