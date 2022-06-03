module Arkham.Skill.Cards.Deduction where

import Arkham.Prelude

import qualified Arkham.Action as Action
import Arkham.Classes
import Arkham.EffectMetadata
import Arkham.Investigator.Attrs (Field(..))
import Arkham.Message
import Arkham.Projection
import Arkham.Skill.Attrs
import qualified Arkham.Skill.Cards as Cards
import Arkham.Target

newtype Deduction = Deduction SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor m, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deduction :: SkillCard Deduction
deduction = skill Deduction Cards.deduction

instance RunMessage Deduction where
  runMessage msg s@(Deduction attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid (Just Action.Investigate) _ (SkillTarget sid) _ _
      | sid == skillId -> do
        mlid <- field InvestigatorLocation iid
        for_ mlid $ \lid -> push $ CreateEffect
          "01039"
          (Just $ EffectMetaTarget (LocationTarget lid))
          (toSource attrs)
          (InvestigatorTarget iid)
        pure s
    _ -> Deduction <$> runMessage msg attrs
