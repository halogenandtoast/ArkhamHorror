module Arkham.Story.Cards.SeismicStomp (seismicStomp) where

import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype SeismicStomp = SeismicStomp StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seismicStomp :: StoryCard SeismicStomp
seismicStomp = story SeismicStomp Cards.seismicStomp

instance RunMessage SeismicStomp where
  runMessage msg s@(SeismicStomp attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      withCthulhuLocation \lid -> do
        investigators <- select $ investigatorAt lid
        if null investigators
          then drawCthulhuDeckCard iid attrs
          else for_ investigators \iid' -> do
            sid <- getRandom
            chooseBeginSkillTest sid iid' attrs iid' [#agility, #willpower] (ScenarioCount CthulhuRage)
      pure s
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOneM iid $ sharedI18n $ countVar 1 do
        labeled' "takeDamage" $ assignDamage iid attrs 1
        labeled' "takeHorror" $ assignHorror iid attrs 1
        labeled' "discardRandomCardsFromHand" $ randomDiscard iid attrs
      pure s
    _ -> SeismicStomp <$> liftRunMessage msg attrs
