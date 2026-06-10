module Arkham.Story.Cards.TheSupplicantsEvidence (theSupplicantsEvidence) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Helpers.Log (remembered)
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheSupplicantsEvidence = TheSupplicantsEvidence StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSupplicantsEvidence :: StoryCard TheSupplicantsEvidence
theSupplicantsEvidence = story TheSupplicantsEvidence Cards.theSupplicantsEvidence & persistStory

instance HasAbilities TheSupplicantsEvidence where
  getAbilities (TheSupplicantsEvidence a) =
    [mkAbility a 1 $ forced $ RememberedLogKey #after SabotagedTheTrain]

instance RunMessage TheSupplicantsEvidence where
  runMessage msg s@(TheSupplicantsEvidence attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      whenM (remembered SabotagedTheTrain) $ resolveEvidence attrs "theSupplicantsEvidence.benefit"
      pure s
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      resolveEvidence attrs "theSupplicantsEvidence.benefit"
      pure s
    _ -> TheSupplicantsEvidence <$> liftRunMessage msg attrs
