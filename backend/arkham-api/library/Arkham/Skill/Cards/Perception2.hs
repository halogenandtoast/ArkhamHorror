module Arkham.Skill.Cards.Perception2 (perception2) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype Perception2 = Perception2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

perception2 :: SkillCard Perception2
perception2 = skill Perception2 Cards.perception2

instance RunMessage Perception2 where
  runMessage msg s@(Perception2 attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy _ n -> do
              let amount = if n >= 2 then 2 else 1
              provideSkillTestResultOption attrs exclusions "Perception (2)" do
                drawCards attrs.owner attrs amount
            _ -> pure ()
      pure s
    _ -> Perception2 <$> liftRunMessage msg attrs
