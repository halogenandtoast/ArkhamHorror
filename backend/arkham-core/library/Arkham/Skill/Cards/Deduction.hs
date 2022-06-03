module Arkham.Skill.Cards.Deduction where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.EffectMetadata
import Arkham.Id
import Arkham.Message
import Arkham.Skill.Attrs
import Arkham.Skill.Runner
import Arkham.Target

newtype Deduction = Deduction SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deduction :: SkillCard Deduction
deduction = skill Deduction Cards.deduction

instance (SkillRunner env) => RunMessage Deduction where
  runMessage msg s@(Deduction attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid (Just Action.Investigate) _ (SkillTarget sid) _ _
      | sid == skillId -> do
        lid <- getId @LocationId iid
        s <$ push
          (CreateEffect
            "01039"
            (Just $ EffectMetaTarget (LocationTarget lid))
            (toSource attrs)
            (InvestigatorTarget iid)
          )
    _ -> Deduction <$> runMessage msg attrs
