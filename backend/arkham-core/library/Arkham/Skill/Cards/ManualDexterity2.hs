module Arkham.Skill.Cards.ManualDexterity2 where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype ManualDexterity2 = ManualDexterity2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manualDexterity2 :: SkillCard ManualDexterity2
manualDexterity2 = skill ManualDexterity2 Cards.manualDexterity2

instance RunMessage ManualDexterity2 where
  runMessage msg s@(ManualDexterity2 attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ n -> do
      drawing <- drawCards skillOwner attrs (if n >= 2 then 2 else 1)
      push drawing
      pure s
    _ -> ManualDexterity2 <$> runMessage msg attrs
