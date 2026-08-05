module Arkham.Story.Cards.RuthlessCharge (ruthlessCharge) where

import Arkham.Draw.Types (CardDraw (..), CardDrawRules (..))
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype RuthlessCharge = RuthlessCharge StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruthlessCharge :: StoryCard RuthlessCharge
ruthlessCharge = story RuthlessCharge Cards.ruthlessCharge

{- | "If no attack was made, the investigator with the highest [willpower] draws the
top card of the encounter deck. That card's effects cannot be canceled and it loses
surge."

Shared because there are two ways for the attack not to happen: nobody is standing
with Cthulhu to be attacked, or the attack was made and then cancelled.
-}
noAttackWasMade :: ReverseQueue m => StoryAttrs -> m ()
noAttackWasMade attrs = do
  investigators <- select $ InvestigatorWithHighestSkill #willpower UneliminatedInvestigator
  leadChooseOrRunOneM do
    targets investigators \iid ->
      drawEncounterCardEdit iid attrs \d ->
        d
          { cardDrawRules =
              singleton $ WithDrawnCardModifiers (toSource attrs) [EffectsCannotBeCanceled, NoSurge]
          }

instance RunMessage RuthlessCharge where
  runMessage msg s@(RuthlessCharge attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      doStep 1 msg
      doStep 2 msg
      doStep 3 msg
      pure s
    DoStep 1 (ResolveThisStory _ (is attrs -> True)) -> do
      whenCthulhuHas HoaryWings do
        eachInvestigatorWithCthulhu (`randomDiscard` attrs)
      pure s
    DoStep 2 (ResolveThisStory _ (is attrs -> True)) -> do
      whenCthulhuHas FierceVisage do
        investigators <-
          getCthulhuLocation >>= \case
            Just lid -> select $ InvestigatorWithHighestSkill #willpower (investigatorAt lid)
            Nothing -> pure []
        if null investigators
          then do_ msg
          else leadChooseOrRunOneM $ targets investigators (`forInvestigator` msg)
      pure s
    ForInvestigator iid inner@(DoStep 2 (ResolveThisStory _ (is attrs -> True))) -> do
      didAttack <- cthulhuFacetAttacks attrs FierceVisage iid
      unless didAttack $ do_ inner
      pure s
    Do (DoStep 2 (ResolveThisStory _ (is attrs -> True))) -> do
      noAttackWasMade attrs
      pure s
    After (EnemyAttack details)
      | isSource attrs details.source
      , details.cancelled -> do
          fierceVisage <- getCthulhuFacet FierceVisage
          when (Just details.enemy == fierceVisage) $ noAttackWasMade attrs
          pure s
    DoStep 3 (ResolveThisStory _ (is attrs -> True)) -> do
      whenCthulhuHas WickedClaw do
        withCthulhuLocation \lid -> do
          investigators <- select $ InvestigatorWithHighestSkill #combat (investigatorAt lid)
          sid <- getRandom
          leadChooseOrRunOneM do
            targets investigators \iid -> beginSkillTest sid iid attrs iid #combat (ScenarioCount CthulhuRage)
      pure s
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      void $ cthulhuFacetAttacks attrs WickedClaw iid
      pure s
    _ -> RuthlessCharge <$> liftRunMessage msg attrs
