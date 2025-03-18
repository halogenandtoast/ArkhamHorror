module Arkham.Skill.Cards.Arrogance (arrogance) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Arrogance = Arrogance SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arrogance :: SkillCard Arrogance
arrogance = skill Arrogance Cards.arrogance

instance RunMessage Arrogance where
  runMessage msg s@(Arrogance attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      skillTestResultOption "Arrogance" $ returnToHand attrs.owner attrs
      pure s
    _ -> Arrogance <$> liftRunMessage msg attrs
