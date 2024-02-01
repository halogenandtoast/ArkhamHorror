module Arkham.Skill.Cards.Opportunist (
  opportunist,
  Opportunist (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Opportunist = Opportunist SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

opportunist :: SkillCard Opportunist
opportunist = skill Opportunist Cards.opportunist

instance RunMessage Opportunist where
  runMessage msg s@(Opportunist attrs) = case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ n | n >= 3 -> do
      push $ ReturnToHand iid (toTarget attrs)
      pure s
    _ -> Opportunist <$> runMessage msg attrs
