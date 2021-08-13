module Arkham.Types.Skill.Cards.TrueUnderstanding
  ( trueUnderstanding
  , TrueUnderstanding(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.Target

newtype TrueUnderstanding = TrueUnderstanding SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueUnderstanding :: SkillCard TrueUnderstanding
trueUnderstanding = skill TrueUnderstanding Cards.trueUnderstanding

-- Investigation is not an ability on the card so we need to pass
-- Nothing for the action type

instance SkillRunner env => RunMessage env TrueUnderstanding where
  runMessage msg s@(TrueUnderstanding attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid _ _ (SkillTarget sid) _ _ | sid == skillId -> do
      lid <- getId iid
      s <$ push (InvestigatorDiscoverClues iid lid 1 Nothing)
    _ -> TrueUnderstanding <$> runMessage msg attrs
