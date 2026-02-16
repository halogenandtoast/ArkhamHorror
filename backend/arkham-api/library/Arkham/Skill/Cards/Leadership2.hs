module Arkham.Skill.Cards.Leadership2 (leadership2) where

import Arkham.Helpers.SkillTest
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype Leadership2 = Leadership2 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadership2 :: SkillCard Leadership2
leadership2 = skill Leadership2 Cards.leadership2

instance HasModifiersFor Leadership2 where
  getModifiersFor (Leadership2 attrs) = do
    withSkillTestInvestigator \iid ->
      addSkillIconsWhen attrs (attrs.owner /= iid) [#willpower, #wild]

instance RunMessage Leadership2 where
  runMessage msg s@(Leadership2 attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              provideSkillTestResultOption attrs exclusions "Leadership (2)" do
                gainResources attrs.owner attrs 2
                when (st.investigator /= attrs.owner) $ gainResources st.investigator attrs 2
            _ -> pure ()
      pure s
    _ -> Leadership2 <$> liftRunMessage msg attrs
