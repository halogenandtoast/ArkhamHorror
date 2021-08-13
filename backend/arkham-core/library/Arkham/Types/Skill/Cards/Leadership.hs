module Arkham.Types.Skill.Cards.Leadership
  ( leadership
  , Leadership(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Modifier
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype Leadership = Leadership SkillAttrs
  deriving anyclass (IsSkill, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadership :: SkillCard Leadership
leadership = skill Leadership Cards.leadership

instance HasModifiersFor env Leadership where
  getModifiersFor (SkillTestSource iid' _ _ _ _) (CardIdTarget cid) (Leadership attrs)
    | toCardId attrs == cid && skillOwner attrs /= iid'
    = pure $ toModifiers attrs [AddSkillIcons [SkillWillpower, SkillWild]]
  getModifiersFor _ _ _ = pure []

instance SkillRunner env => RunMessage env Leadership where
  runMessage msg (Leadership attrs) = Leadership <$> runMessage msg attrs
