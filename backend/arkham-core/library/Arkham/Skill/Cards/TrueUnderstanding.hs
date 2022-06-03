module Arkham.Skill.Cards.TrueUnderstanding
  ( trueUnderstanding
  , TrueUnderstanding(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Attrs
import Arkham.Skill.Runner
import Arkham.Target

newtype TrueUnderstanding = TrueUnderstanding SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueUnderstanding :: SkillCard TrueUnderstanding
trueUnderstanding = skill TrueUnderstanding Cards.trueUnderstanding

-- Investigation is not an ability on the card so we need to pass
-- Nothing for the action type

instance SkillRunner env => RunMessage TrueUnderstanding where
  runMessage msg s@(TrueUnderstanding attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid _ _ (SkillTarget sid) _ _ | sid == skillId -> do
      lid <- getId iid
      s <$ push (InvestigatorDiscoverClues iid lid 1 Nothing)
    _ -> TrueUnderstanding <$> runMessage msg attrs
