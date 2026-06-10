module Arkham.Story.Cards.TheProfessorsEvidence (theProfessorsEvidence) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Helpers.Log (remembered)
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheProfessorsEvidence = TheProfessorsEvidence StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theProfessorsEvidence :: StoryCard TheProfessorsEvidence
theProfessorsEvidence = story TheProfessorsEvidence Cards.theProfessorsEvidence & persistStory

instance HasAbilities TheProfessorsEvidence where
  getAbilities (TheProfessorsEvidence a) =
    [mkAbility a 1 $ forced $ RememberedLogKey #after BoughtAnOddTrinket]

instance RunMessage TheProfessorsEvidence where
  runMessage msg s@(TheProfessorsEvidence attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      whenM (remembered BoughtAnOddTrinket) $ resolveEvidence attrs "theProfessorsEvidence.benefit"
      pure s
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      resolveEvidence attrs "theProfessorsEvidence.benefit"
      pure s
    _ -> TheProfessorsEvidence <$> liftRunMessage msg attrs
