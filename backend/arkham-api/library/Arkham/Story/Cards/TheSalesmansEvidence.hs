module Arkham.Story.Cards.TheSalesmansEvidence (theSalesmansEvidence) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Helpers.Log (remembered)
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheSalesmansEvidence = TheSalesmansEvidence StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSalesmansEvidence :: StoryCard TheSalesmansEvidence
theSalesmansEvidence = story TheSalesmansEvidence Cards.theSalesmansEvidence & persistStory

instance HasAbilities TheSalesmansEvidence where
  getAbilities (TheSalesmansEvidence a) =
    [mkAbility a 1 $ forced $ RememberedLogKey #after BrokenIntoADesertedTemple]

instance RunMessage TheSalesmansEvidence where
  runMessage msg s@(TheSalesmansEvidence attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      whenM (remembered BrokenIntoADesertedTemple) $ resolveEvidence attrs "theSalesmansEvidence.benefit"
      pure s
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      resolveEvidence attrs "theSalesmansEvidence.benefit"
      pure s
    _ -> TheSalesmansEvidence <$> liftRunMessage msg attrs
