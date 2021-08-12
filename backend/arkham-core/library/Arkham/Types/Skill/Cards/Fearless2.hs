module Arkham.Types.Skill.Cards.Fearless2 where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.Target

newtype Fearless2 = Fearless2 SkillAttrs
  deriving anyclass IsSkill
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fearless2 :: SkillCard Fearless2
fearless2 = skill Fearless2 Cards.fearless2

instance HasModifiersFor env Fearless2
instance HasActions Fearless2

instance (SkillRunner env) => RunMessage env Fearless2 where
  runMessage msg s@(Fearless2 attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ n | sid == skillId -> s <$ push
      (HealHorror (InvestigatorTarget skillOwner) (if n >= 2 then 2 else 1))
    _ -> Fearless2 <$> runMessage msg attrs
