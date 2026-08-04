module Arkham.Story.Cards.PsychicRebuke (psychicRebuke) where

import Arkham.Helpers.Scenario (getVictoryDisplay)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Card
import Arkham.Helpers.Query (getLead)
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Trait (Trait (Cthulhu))

newtype PsychicRebuke = PsychicRebuke StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychicRebuke :: StoryCard PsychicRebuke
psychicRebuke = story PsychicRebuke Cards.psychicRebuke

instance RunMessage PsychicRebuke where
  runMessage msg s@(PsychicRebuke attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      banished <- filter (`cardMatch` CardWithTrait Cthulhu) <$> getVictoryDisplay
      if null banished
        then
          -- "If there are no [[Cthulhu]] enemies in the victory display, discard this
          -- card and draw the top card of the Cthulhu deck."
          drawCthulhuDeckCard iid attrs
        else do
          -- "Otherwise, choose an investigator to test [willpower] (X), where X is
          -- Cthulhu's Rage."
          rage <- getCthulhuRage
          lead <- getLead
          investigators <- select UneliminatedInvestigator
          chooseOrRunOneM lead $ scenarioI18n do
            questionLabeled' "chooseInvestigatorToTest"
            targets investigators \iid' -> do
              sid <- getRandom
              onFailedByEffect sid AnyValue attrs iid' $ doStep 1 msg
              beginSkillTest sid iid' attrs iid' #willpower (Fixed rage)
      pure s
    {- "If they fail, choose a [[Cthulhu]] enemy in the victory display and return it
    to the Cthulhu board, [[Enraged]] side faceup." The victory display holds whichever
    face was showing when the facet was banished, so the return is looked up through
    the front/Enraged pairing rather than replayed from the stored card. -}
    DoStep 1 (ResolveThisStory _ (is attrs -> True)) -> do
      banished <- filter (`cardMatch` CardWithTrait Cthulhu) <$> getVictoryDisplay
      getCthulhuLocation >>= traverse_ \lid -> do
        lead <- getLead
        chooseOrRunOneM lead $ scenarioI18n do
          questionLabeled' "chooseCthulhuEnemyToReturn"
          for_ banished \card ->
            for_
              (find (\(front, enraged) -> card `cardMatch` mapOneOf cardIs [front, enraged]) cthulhuFacets)
              \(_front, enraged) -> cardLabeled card do
                obtainCard card
                createEnemyAt_ enraged lid
      pure s
    _ -> PsychicRebuke <$> liftRunMessage msg attrs
