module Arkham.Skill.Cards.Guts2 (guts2) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype Guts2 = Guts2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guts2 :: SkillCard Guts2
guts2 = skill Guts2 Cards.guts2

instance RunMessage Guts2 where
  runMessage msg s@(Guts2 attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy _ n -> do
              let amount = if n >= 2 then 2 else 1
              provideSkillTestResultOption attrs exclusions "Guts (2)" do
                drawCards attrs.owner attrs amount
            _ -> pure ()
      pure s
    _ -> Guts2 <$> liftRunMessage msg attrs
