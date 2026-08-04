module Arkham.Story.Cards.SeismicStomp (seismicStomp) where

import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
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
      rage <- getCthulhuRage
      getCthulhuLocation >>= traverse_ \lid -> do
        investigators <- select $ investigatorAt lid
        if null investigators
          then
            -- "If there are no investigators at Cthulhu's location, discard this card
            -- and draw the top card of the Cthulhu deck." The scenario already queues
            -- the discard behind this resolution.
            drawCthulhuDeckCard iid attrs
          else
            {- "Otherwise, each investigator at Cthulhu's location tests [agility] or
            [willpower] (X), where X is Cthulhu's Rage. If they fail, they must either
            take 1 damage/horror, or discard 1 card from their hand at random." -}
            for_ investigators \iid' -> do
              sid <- getRandom
              onFailedByEffect sid AnyValue attrs iid' $ chooseOneM iid' $ sharedI18n $ countVar 1 do
                labeled' "takeDamage" $ assignDamage iid' attrs 1
                labeled' "takeHorror" $ assignHorror iid' attrs 1
                labeled' "discardRandomCardsFromHand" $ randomDiscard iid' attrs
              chooseOneM iid' do
                scenarioI18n $ questionLabeled' "chooseSkillForTest"
                sharedI18n do
                  labeled' "chooseAgility" $ beginSkillTest sid iid' attrs iid' #agility (Fixed rage)
                  labeled' "chooseWillpower" $ beginSkillTest sid iid' attrs iid' #willpower (Fixed rage)
      pure s
    _ -> SeismicStomp <$> liftRunMessage msg attrs
