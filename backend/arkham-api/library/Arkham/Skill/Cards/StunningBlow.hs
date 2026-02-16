module Arkham.Skill.Cards.StunningBlow (stunningBlow) where

import Arkham.Action
import Arkham.Helpers.SkillTest
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype StunningBlow = StunningBlow SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stunningBlow :: SkillCard StunningBlow
stunningBlow = skill StunningBlow Cards.stunningBlow

instance RunMessage StunningBlow where
  runMessage msg s@(StunningBlow attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              when (st.action == Just Fight) do
                provideSkillTestResultOption attrs exclusions "Stunning Blow" do
                  withSkillTestEnemyTarget (automaticallyEvadeEnemy st.investigator)
            _ -> pure ()
      pure s
    _ -> StunningBlow <$> liftRunMessage msg attrs
