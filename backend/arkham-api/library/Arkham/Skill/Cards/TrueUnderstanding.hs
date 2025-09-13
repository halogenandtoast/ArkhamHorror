module Arkham.Skill.Cards.TrueUnderstanding (trueUnderstanding) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype TrueUnderstanding = TrueUnderstanding SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueUnderstanding :: SkillCard TrueUnderstanding
trueUnderstanding = skill TrueUnderstanding Cards.trueUnderstanding

instance RunMessage TrueUnderstanding where
  runMessage msg s@(TrueUnderstanding attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      discoverAtYourLocation NotInvestigate iid attrs 1
      pure s
    _ -> TrueUnderstanding <$> liftRunMessage msg attrs
