module Arkham.Story.Cards.ThePriestesssEvidence (thePriestesssEvidence) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Helpers.Log (remembered)
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ThePriestesssEvidence = ThePriestesssEvidence StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePriestesssEvidence :: StoryCard ThePriestesssEvidence
thePriestesssEvidence = story ThePriestesssEvidence Cards.thePriestesssEvidence & persistStory

instance HasAbilities ThePriestesssEvidence where
  getAbilities (ThePriestesssEvidence a) =
    [mkAbility a 1 $ forced $ RememberedLogKey #after FoundADoorMarkedWithBlood]

instance RunMessage ThePriestesssEvidence where
  runMessage msg s@(ThePriestesssEvidence attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      whenM (remembered FoundADoorMarkedWithBlood) $ resolveEvidence attrs "thePriestesssEvidence.benefit"
      pure s
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      resolveEvidence attrs "thePriestesssEvidence.benefit"
      pure s
    _ -> ThePriestesssEvidence <$> liftRunMessage msg attrs
