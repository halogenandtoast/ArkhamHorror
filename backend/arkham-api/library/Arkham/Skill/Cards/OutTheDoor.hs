module Arkham.Skill.Cards.OutTheDoor (outTheDoor) where

import Arkham.Helpers.SkillTest (withSkillTestInvestigator)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype OutTheDoor = OutTheDoor SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outTheDoor :: SkillCard OutTheDoor
outTheDoor = skill OutTheDoor Cards.outTheDoor

instance RunMessage OutTheDoor where
  runMessage msg s@(OutTheDoor attrs) = runQueueT $ case msg of
    Do (CommitCard _iid card) | attrs.cardId == card.id -> do
      withSkillTestInvestigator \iid -> gainResources iid attrs 2
      OutTheDoor <$> liftRunMessage msg attrs
    FailedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      loseResources iid attrs 2
      pure s
    _ -> OutTheDoor <$> liftRunMessage msg attrs
