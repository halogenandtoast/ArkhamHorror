module Arkham.Story.Cards.DireGale (direGale) where

import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DireGale = DireGale StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

direGale :: StoryCard DireGale
direGale = story DireGale Cards.direGale

instance RunMessage DireGale where
  runMessage msg s@(DireGale attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      doStep 1 msg
      doStep 2 msg
      doStep 3 msg
      pure s
    DoStep 1 (ResolveThisStory _ (is attrs -> True)) -> do
      whenCthulhuHas HoaryWings do
        eachInvestigatorWithCthulhu (`randomDiscard` attrs)
        resolveCthulhuPatrol
      pure s
    DoStep 2 (ResolveThisStory _ (is attrs -> True)) -> do
      whenCthulhuHas FierceVisage do
        investigators <- select $ InvestigatorWithHighestSkill #willpower UneliminatedInvestigator
        sid <- getRandom
        leadChooseOrRunOneM do
          targets investigators \iid ->
            beginSkillTest sid iid attrs iid #willpower (ScenarioCount CthulhuRage)
      pure s
    DoStep 3 (ResolveThisStory _ (is attrs -> True)) -> do
      whenCthulhuHas WickedClaw do
        unlessM cthulhuPatrolledThisRound do
          eachInvestigatorWithCthulhu $ void . cthulhuFacetAttacks attrs WickedClaw
      pure s
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      nonStory <- selectAny $ assetControlledBy iid <> AssetNonStory <> DiscardableAsset
      chooseOneM iid $ sharedI18n $ countVar 1 do
        labeled' "takeHorror" $ assignHorror iid attrs 1
        labeledValidate' nonStory "discardAssets"
          $ chooseAndDiscardAssetMatching iid attrs AssetNonStory
      pure s
    _ -> DireGale <$> liftRunMessage msg attrs
