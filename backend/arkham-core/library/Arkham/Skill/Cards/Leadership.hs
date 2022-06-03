module Arkham.Skill.Cards.Leadership
  ( leadership
  , Leadership(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Modifier
import Arkham.Skill.Attrs
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype Leadership = Leadership SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadership :: SkillCard Leadership
leadership = skill Leadership Cards.leadership

instance HasModifiersFor env Leadership where
  getModifiersFor (SkillTestSource iid' _ _ _) (CardIdTarget cid) (Leadership attrs)
    | toCardId attrs == cid && skillOwner attrs /= iid'
    = pure $ toModifiers attrs [AddSkillIcons [SkillWillpower, SkillWild]]
  getModifiersFor _ _ _ = pure []

instance RunMessage Leadership where
  runMessage msg (Leadership attrs) = Leadership <$> runMessage msg attrs
