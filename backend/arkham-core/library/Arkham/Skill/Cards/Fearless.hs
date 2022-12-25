module Arkham.Skill.Cards.Fearless where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Damage
import Arkham.Matcher
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
import Arkham.Target

newtype Fearless = Fearless SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fearless :: SkillCard Fearless
fearless = skill Fearless Cards.fearless

instance RunMessage Fearless where
  runMessage msg s@(Fearless attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ _ | sid == skillId -> do
      isHealable <-
        selectAny
        $ HealableInvestigator (toSource attrs) HorrorType
        $ InvestigatorWithId skillOwner
      when isHealable
        $ push (HealHorror (InvestigatorTarget skillOwner) (toSource attrs) 1)
      pure s
    _ -> Fearless <$> runMessage msg attrs
