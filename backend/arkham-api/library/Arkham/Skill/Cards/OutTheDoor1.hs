module Arkham.Skill.Cards.OutTheDoor1 (outTheDoor1) where

import Arkham.Helpers.SkillTest (withSkillTestInvestigator)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype OutTheDoor1 = OutTheDoor1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outTheDoor1 :: SkillCard OutTheDoor1
outTheDoor1 = skill OutTheDoor1 Cards.outTheDoor1

instance RunMessage OutTheDoor1 where
  runMessage msg s@(OutTheDoor1 attrs) = runQueueT $ case msg of
    Do (CommitCard _iid card) | attrs.cardId == card.id -> do
      withSkillTestInvestigator \iid -> gainResources iid attrs 4
      OutTheDoor1 <$> liftRunMessage msg attrs
    FailedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      loseResources iid attrs 4
      pure s
    _ -> OutTheDoor1 <$> liftRunMessage msg attrs
