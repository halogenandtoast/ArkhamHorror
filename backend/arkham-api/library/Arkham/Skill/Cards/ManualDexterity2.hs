module Arkham.Skill.Cards.ManualDexterity2 (manualDexterity2) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype ManualDexterity2 = ManualDexterity2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manualDexterity2 :: SkillCard ManualDexterity2
manualDexterity2 = skill ManualDexterity2 Cards.manualDexterity2

instance RunMessage ManualDexterity2 where
  runMessage msg s@(ManualDexterity2 attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy _ n -> do
              provideSkillTestResultOption attrs exclusions "Manual Dexterity (2)" do
                drawCards attrs.owner attrs (if n >= 2 then 2 else 1)
            _ -> pure ()
      pure s
    _ -> ManualDexterity2 <$> liftRunMessage msg attrs
