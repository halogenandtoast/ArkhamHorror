module Arkham.Story.Cards.Dreadsight (dreadsight) where

import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype Dreadsight = Dreadsight StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreadsight :: StoryCard Dreadsight
dreadsight = story Dreadsight Cards.dreadsight

instance RunMessage Dreadsight where
  runMessage msg s@(Dreadsight attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      doStep 1 msg
      doStep 2 msg
      doStep 3 msg
      pure s
    DoStep 1 (ResolveThisStory _ (is attrs -> True)) -> do
      whenCthulhuHas HoaryWings do
        investigators <- select $ InvestigatorWithHighestSkill #agility UneliminatedInvestigator
        sid <- getRandom
        leadChooseOrRunOneM do
          targets investigators \iid -> beginSkillTest sid iid attrs iid #agility (ScenarioCount CthulhuRage)
      pure s
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOneM iid $ sharedI18n $ countVar 1 do
        labeled' "takeDamage" $ assignDamage iid attrs 1
        labeled' "takeHorror" $ assignHorror iid attrs 1
      pure s
    DoStep 2 (ResolveThisStory _ (is attrs -> True)) -> do
      whenCthulhuHas FierceVisage $ eachInvestigatorWithCthulhu \iid -> loseResources iid attrs 2
      pure s
    DoStep 3 (ResolveThisStory _ (is attrs -> True)) -> do
      whenCthulhuHas WickedClaw do
        attacked <- getInvestigatorsWithCthulhu
        leadChooseOneM do
          scenarioI18n $ questionLabeled' "dreadsightGroupChoice"
          sharedI18n $ countVar 1 $ labeled' "loseActions" do
            eachInvestigator \iid -> nextTurnModifier iid attrs iid (FewerActions 1)
          scenarioI18n $ labeledValidate' (notNull attacked) "cthulhuAttacks" do
            for_ attacked $ void . cthulhuFacetAttacks attrs WickedClaw
          sharedI18n $ countVar 1 $ labeled' "takeDirectHorror" do
            eachInvestigator \iid -> directHorror iid attrs 1
      pure s
    _ -> Dreadsight <$> liftRunMessage msg attrs
