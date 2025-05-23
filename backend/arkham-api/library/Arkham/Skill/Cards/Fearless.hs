module Arkham.Skill.Cards.Fearless (fearless) where

import Arkham.Helpers.Investigator
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Fearless = Fearless SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fearless :: SkillCard Fearless
fearless = skill Fearless Cards.fearless

instance RunMessage Fearless where
  runMessage msg s@(Fearless attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      whenM (canHaveHorrorHealed attrs attrs.owner) $ healHorror attrs.owner attrs 1
      pure s
    _ -> Fearless <$> liftRunMessage msg attrs
