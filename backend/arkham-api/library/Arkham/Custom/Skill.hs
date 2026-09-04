-- | The runner behind a debug-authored custom skill. See "Arkham.Custom.Enemy".
module Arkham.Custom.Skill (CustomSkill (..), customSkill) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Skill.Import.Lifted

newtype CustomSkill = CustomSkill SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

customSkill :: CardDef -> SkillCard CustomSkill
customSkill = skill CustomSkill

instance RunMessage CustomSkill where
  runMessage msg (CustomSkill attrs) = CustomSkill <$> runMessage msg attrs
