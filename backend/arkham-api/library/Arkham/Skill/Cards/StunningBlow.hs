module Arkham.Skill.Cards.StunningBlow (stunningBlow) where

import Arkham.Action
import Arkham.Helpers.SkillTest
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype StunningBlow = StunningBlow SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stunningBlow :: SkillCard StunningBlow
stunningBlow = skill StunningBlow Cards.stunningBlow

instance RunMessage StunningBlow where
  runMessage msg s@(StunningBlow attrs) = runQueueT $ case msg of
    PassedSkillTest iid (Just Fight) _ (SkillTarget sid) _ _ | sid == toId attrs -> do
      skillTestResultOption "Stunning Blow" do
        withSkillTestEnemyTarget (automaticallyEvadeEnemy iid)
      pure s
    _ -> StunningBlow <$> liftRunMessage msg attrs
