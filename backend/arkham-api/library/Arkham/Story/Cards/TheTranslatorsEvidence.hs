module Arkham.Story.Cards.TheTranslatorsEvidence (theTranslatorsEvidence) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Helpers.Log (remembered)
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheTranslatorsEvidence = TheTranslatorsEvidence StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTranslatorsEvidence :: StoryCard TheTranslatorsEvidence
theTranslatorsEvidence = story TheTranslatorsEvidence Cards.theTranslatorsEvidence & persistStory

instance HasAbilities TheTranslatorsEvidence where
  getAbilities (TheTranslatorsEvidence a) =
    [mkAbility a 1 $ forced $ RememberedLogKey #after DiscoveredAnAncientTablet]

instance RunMessage TheTranslatorsEvidence where
  runMessage msg s@(TheTranslatorsEvidence attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      whenM (remembered DiscoveredAnAncientTablet) $ resolveEvidence attrs "theTranslatorsEvidence.benefit"
      pure s
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      resolveEvidence attrs "theTranslatorsEvidence.benefit"
      pure s
    _ -> TheTranslatorsEvidence <$> liftRunMessage msg attrs
