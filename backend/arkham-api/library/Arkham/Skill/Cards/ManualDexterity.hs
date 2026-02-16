module Arkham.Skill.Cards.ManualDexterity (manualDexterity) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype ManualDexterity = ManualDexterity SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manualDexterity :: SkillCard ManualDexterity
manualDexterity = skill ManualDexterity Cards.manualDexterity

instance RunMessage ManualDexterity where
  runMessage msg s@(ManualDexterity attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              provideSkillTestResultOption attrs exclusions "Manual Dexterity" do
                drawCards attrs.owner attrs 1
            _ -> pure ()
      pure s
    _ -> ManualDexterity <$> liftRunMessage msg attrs
