module Arkham.Skill.Cards.Eureka (eureka) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype Eureka = Eureka SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eureka :: SkillCard Eureka
eureka = skill Eureka Cards.eureka

instance RunMessage Eureka where
  runMessage msg s@(Eureka attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              provideSkillTestResultOption attrs exclusions "Eureka" do
                search st.investigator attrs st.investigator [fromTopOfDeck 3] #any (DrawFound st.investigator 1)
            _ -> pure ()
      pure s
    _ -> Eureka <$> liftRunMessage msg attrs
