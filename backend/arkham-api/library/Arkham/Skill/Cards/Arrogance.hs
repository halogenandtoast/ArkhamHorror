module Arkham.Skill.Cards.Arrogance (arrogance) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype Arrogance = Arrogance SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arrogance :: SkillCard Arrogance
arrogance = skill Arrogance Cards.arrogance

instance RunMessage Arrogance where
  runMessage msg s@(Arrogance attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              provideSkillTestResultOption attrs exclusions "Arrogance" $ returnToHand attrs.owner attrs
            _ -> pure ()
      pure s
    _ -> Arrogance <$> liftRunMessage msg attrs
