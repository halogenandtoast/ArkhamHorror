module Arkham.Skill.Cards.Opportunist (opportunist) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype Opportunist = Opportunist SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

opportunist :: SkillCard Opportunist
opportunist = skill Opportunist Cards.opportunist

instance RunMessage Opportunist where
  runMessage msg s@(Opportunist attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy _ n | n >= 3 -> do
              provideSkillTestResultOption attrs exclusions "Opportunist" do
                returnToHand st.investigator attrs
            _ -> pure ()
      pure s
    _ -> Opportunist <$> liftRunMessage msg attrs
