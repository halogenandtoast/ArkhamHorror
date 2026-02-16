module Arkham.Skill.Cards.Opportunist2 (opportunist2) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype Opportunist2 = Opportunist2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

opportunist2 :: SkillCard Opportunist2
opportunist2 = skill Opportunist2 Cards.opportunist2

instance RunMessage Opportunist2 where
  runMessage msg s@(Opportunist2 attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy _ n | n >= 2 -> do
              provideSkillTestResultOption attrs exclusions "Opportunist (2)" do
                returnToHand st.investigator attrs
            _ -> pure ()
      pure s
    _ -> Opportunist2 <$> liftRunMessage msg attrs
