module Arkham.Skill.Cards.Fearless2 (fearless2) where

import Arkham.Helpers.Investigator
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Fearless2 = Fearless2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fearless2 :: SkillCard Fearless2
fearless2 = skill Fearless2 Cards.fearless2

instance RunMessage Fearless2 where
  runMessage msg s@(Fearless2 attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ n -> do
      whenM (canHaveHorrorHealed attrs attrs.owner) do
        healHorror attrs.owner attrs $ if n >= 2 then 2 else 1
      pure s
    _ -> Fearless2 <$> liftRunMessage msg attrs
