module Arkham.Skill.Cards.TrueUnderstanding (
  trueUnderstanding,
  TrueUnderstanding (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Message
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype TrueUnderstanding = TrueUnderstanding SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueUnderstanding :: SkillCard TrueUnderstanding
trueUnderstanding = skill TrueUnderstanding Cards.trueUnderstanding

-- Investigation is not an ability on the card so we need to pass
-- Nothing for the action type

instance RunMessage TrueUnderstanding where
  runMessage msg s@(TrueUnderstanding attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid _ _ (SkillTarget sid) _ _ | sid == skillId -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid $ \lid -> do
        push $ InvestigatorDiscoverClues iid lid (toSource attrs) 1 Nothing
      pure s
    _ -> TrueUnderstanding <$> runMessage msg attrs
