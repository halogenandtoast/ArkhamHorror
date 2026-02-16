module Arkham.Skill.Cards.TrueUnderstanding (trueUnderstanding) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype TrueUnderstanding = TrueUnderstanding SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueUnderstanding :: SkillCard TrueUnderstanding
trueUnderstanding = skill TrueUnderstanding Cards.trueUnderstanding

instance RunMessage TrueUnderstanding where
  runMessage msg s@(TrueUnderstanding attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              provideSkillTestResultOption attrs exclusions "True Understanding" do
                discoverAtYourLocation NotInvestigate attrs.owner attrs 1
            _ -> pure ()
      pure s
    _ -> TrueUnderstanding <$> liftRunMessage msg attrs
