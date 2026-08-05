module Arkham.Story.Cards.EldritchCall (eldritchCall) where

import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype EldritchCall = EldritchCall StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eldritchCall :: StoryCard EldritchCall
eldritchCall = story EldritchCall Cards.eldritchCall

instance RunMessage EldritchCall where
  runMessage msg s@(EldritchCall attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      withCthulhuLocation \lid -> do
        investigators <- select $ investigatorAt lid
        if null investigators
          then drawCthulhuDeckCard iid attrs
          else do
            for_ investigators \iid' -> do
              sid <- getRandom
              beginSkillTest sid iid' attrs iid' #willpower (ScenarioCount CthulhuRage)
            doStep 1 msg
      pure s
    FailedThisSkillTest _iid (isSource attrs -> True) -> do
      pure $ s & setMeta True
    DoStep 1 (ResolveThisStory _iid (is attrs -> True)) -> do
      when (toResultDefault False attrs.meta) do
        lead <- getLead
        discardUntilFirst lead attrs Deck.EncounterDeck #enemy
      pure s
    RequestedEncounterCard (isSource attrs -> True) _ (Just card) -> do
      withCthulhuLocation (createEnemyAt_ card)
      pure s
    _ -> EldritchCall <$> liftRunMessage msg attrs
