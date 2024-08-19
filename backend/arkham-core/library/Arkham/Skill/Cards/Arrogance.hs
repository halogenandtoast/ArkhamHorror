module Arkham.Skill.Cards.Arrogance (arrogance, Arrogance (..)) where

import Arkham.Classes
import Arkham.Message
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Arrogance = Arrogance SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arrogance :: SkillCard Arrogance
arrogance = skill Arrogance Cards.arrogance

instance RunMessage Arrogance where
  runMessage msg s@(Arrogance attrs) = case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      push $ ReturnToHand (skillOwner attrs) (toTarget attrs)
      pure s
    _ -> Arrogance <$> runMessage msg attrs
