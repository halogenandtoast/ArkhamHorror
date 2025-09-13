module Arkham.Skill.Cards.Daredevil2 (daredevil2) where

import Arkham.Card
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Daredevil2 = Daredevil2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daredevil2 :: SkillCard Daredevil2
daredevil2 = skill Daredevil2 Cards.daredevil2

instance RunMessage Daredevil2 where
  runMessage msg s@(Daredevil2 attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      revealUntilFirst iid attrs iid
        $ CommittableCard (InvestigatorWithId iid) (basic $ #rogue <> #skill)
      Daredevil2 <$> liftRunMessage msg attrs
    RevealedCards iid (isSource attrs -> True) _ mcard (map toCard -> rest) -> do
      focusCards (rest <> maybeToList mcard) do
        case mcard of
          Nothing -> promptI_ iid "noCardsFound"
          Just (toCard -> c) -> chooseOneM iid do
            targeting (toCardId c) $ commitCard iid c
      shuffleCardsIntoDeck iid rest
      pure s
    _ -> Daredevil2 <$> liftRunMessage msg attrs
