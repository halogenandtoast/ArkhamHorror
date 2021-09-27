module Arkham.Types.Skill.Cards.Deduction where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Types.Action qualified as Action
import Arkham.Types.Classes
import Arkham.Types.EffectMetadata
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.Target

newtype Deduction = Deduction SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deduction :: SkillCard Deduction
deduction = skill Deduction Cards.deduction

instance (SkillRunner env) => RunMessage env Deduction where
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
