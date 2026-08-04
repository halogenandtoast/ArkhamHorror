module Arkham.Story.Cards.Dreadsight (dreadsight) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Helpers.Query (getLead)
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
      rage <- getCthulhuRage

      {- "If Cthulhu (Hoary Wings) is in play: The investigator with the highest
      [agility] tests [agility] (X), where X is Cthulhu's Rage. If they fail, they
      take 1 damage or 1 horror." -}
      whenM (selectAny $ cthulhuFacet Enemies.cthulhuHoaryWings) do
        selectEach (InvestigatorWithHighestSkill #agility UneliminatedInvestigator) \iid -> do
          sid <- getRandom
          onFailedByEffect sid AnyValue attrs iid $ chooseOneM iid $ sharedI18n $ countVar 1 do
            labeled' "takeDamage" $ assignDamage iid attrs 1
            labeled' "takeHorror" $ assignHorror iid attrs 1
          beginSkillTest sid iid attrs iid #agility (Fixed rage)

      -- "If Cthulhu (Fierce Visage) is in play: Each investigator at Cthulhu's
      -- location loses 2 resources."
      whenM (selectAny $ cthulhuFacet Enemies.cthulhuFierceVisage) do
        getCthulhuLocation >>= traverse_ \lid ->
          selectEach (investigatorAt lid) \iid -> loseResources iid attrs 2

      {- "If Cthulhu (Wicked Claw) is in play: The investigators must decide, as a
      group: Each investigator loses 1 action during their next turn. / Cthulhu
      (Wicked Claw) attacks each investigator at its location. / Each investigator
      takes 1 direct horror." -}
      whenM (selectAny $ cthulhuFacet Enemies.cthulhuWickedClaw) do
        lead <- getLead
        chooseOneM lead do
          scenarioI18n $ questionLabeled' "dreadsightGroupChoice"
          sharedI18n $ countVar 1 $ labeled' "loseActions" do
            selectEach UneliminatedInvestigator \iid ->
              nextTurnModifier iid attrs iid (FewerActions 1)
          scenarioI18n $ labeled' "cthulhuAttacks" do
            getCthulhuLocation >>= traverse_ \lid ->
              selectEach (investigatorAt lid)
                $ void
                . cthulhuFacetAttacks attrs Enemies.cthulhuWickedClaw
          sharedI18n $ countVar 1 $ labeled' "takeDirectHorror" do
            selectEach UneliminatedInvestigator \iid -> directHorror iid attrs 1
      pure s
    _ -> Dreadsight <$> liftRunMessage msg attrs
