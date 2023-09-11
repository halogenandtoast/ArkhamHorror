module Arkham.Skill.Cards.Guts where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Guts = Guts SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guts :: SkillCard Guts
guts = skill Guts Cards.guts

instance RunMessage Guts where
  runMessage msg s@(Guts attrs) = case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      pushM $ drawCards (skillOwner attrs) attrs 1
      pure s
    _ -> Guts <$> runMessage msg attrs
