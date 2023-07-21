module Arkham.Skill.Cards.StunningBlow (
  stunningBlow,
  StunningBlow (..),
) where

import Arkham.Prelude

import Arkham.Action
import Arkham.Classes
import Arkham.Helpers.SkillTest
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype StunningBlow = StunningBlow SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stunningBlow :: SkillCard StunningBlow
stunningBlow = skill StunningBlow Cards.stunningBlow

instance RunMessage StunningBlow where
  runMessage msg s@(StunningBlow attrs) = case msg of
    PassedSkillTest iid (Just Fight) _ (SkillTarget sid) _ _ | sid == toId attrs ->
      do
        mSkillTestTarget <- getSkillTestTarget
        for_ mSkillTestTarget $ \case
          EnemyTarget eid -> push $ EnemyEvaded iid eid
          _ -> pure ()
        pure s
    _ -> StunningBlow <$> runMessage msg attrs
