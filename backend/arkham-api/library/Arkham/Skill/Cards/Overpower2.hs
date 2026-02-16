module Arkham.Skill.Cards.Overpower2 (overpower2) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype Overpower2 = Overpower2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overpower2 :: SkillCard Overpower2
overpower2 = skill Overpower2 Cards.overpower2

instance RunMessage Overpower2 where
  runMessage msg s@(Overpower2 attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy _ n -> do
              let amount = if n >= 2 then 2 else 1
              provideSkillTestResultOption attrs exclusions "Overpower (2)" do
                drawCards (skillOwner attrs) attrs amount
            _ -> pure ()
      pure s
    _ -> Overpower2 <$> liftRunMessage msg attrs
