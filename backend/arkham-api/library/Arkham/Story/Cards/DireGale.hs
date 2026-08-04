module Arkham.Story.Cards.DireGale (direGale) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
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
      rage <- getCthulhuRage

      {- "If Cthulhu (Hoary Wings) is in play: Each investigator at Cthulhu's
      location discards 1 card at random from hand. Resolve Cthulhu's patrol keyword
      an additional time." -}
      whenM (selectAny $ cthulhuFacet Enemies.cthulhuHoaryWings) do
        getCthulhuLocation >>= traverse_ \lid ->
          selectEach (investigatorAt lid) (`randomDiscard` attrs)
        resolveCthulhuPatrol

      {- "If Cthulhu (Fierce Visage) is in play: The investigator with the highest
      [willpower] tests [willpower] (X), where X is Cthulhu's Rage. If they fail,
      they must either take 1 horror, or choose and discard a non-story asset they
      control." -}
      whenM (selectAny $ cthulhuFacet Enemies.cthulhuFierceVisage) do
        selectEach (InvestigatorWithHighestSkill #willpower UneliminatedInvestigator) \iid -> do
          sid <- getRandom
          {- The on-fail body is baked when the rider is registered, so the choice is
          deferred a step: what the investigator controls has to be read after the
          test resolves, not before it. -}
          onFailedByEffect sid AnyValue attrs iid $ forInvestigator iid msg
          beginSkillTest sid iid attrs iid #willpower (Fixed rage)

      {- "If Cthulhu (Wicked Claw) is in play: If Cthulhu did not move via his patrol
      keyword this round, Cthulhu (Wicked Claw) attacks each investigator at its
      location." -}
      whenM (selectAny $ cthulhuFacet Enemies.cthulhuWickedClaw) do
        unlessM cthulhuPatrolledThisRound do
          withCthulhuLocation \lid ->
            selectEach (investigatorAt lid) (void . cthulhuFacetAttacks attrs Enemies.cthulhuWickedClaw)
      pure s
    ForInvestigator iid (ResolveThisStory _ (is attrs -> True)) -> do
      nonStory <- selectAny $ assetControlledBy iid <> AssetNonStory <> DiscardableAsset
      chooseOneM iid $ sharedI18n $ countVar 1 do
        labeled' "takeHorror" $ assignHorror iid attrs 1
        labeledValidate' nonStory "discardAssets"
          $ chooseAndDiscardAssetMatching iid attrs AssetNonStory
      pure s
    _ -> DireGale <$> liftRunMessage msg attrs
