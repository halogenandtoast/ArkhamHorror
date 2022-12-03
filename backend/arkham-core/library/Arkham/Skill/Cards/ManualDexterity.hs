module Arkham.Skill.Cards.ManualDexterity where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Runner

newtype ManualDexterity = ManualDexterity SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manualDexterity :: SkillCard ManualDexterity
manualDexterity = skill ManualDexterity Cards.manualDexterity

instance RunMessage ManualDexterity where
  runMessage msg s@(ManualDexterity attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      drawing <- drawCards skillOwner attrs 1
      push drawing
      pure s
    _ -> ManualDexterity <$> runMessage msg attrs
