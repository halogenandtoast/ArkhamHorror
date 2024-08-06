module Arkham.Skill.Cards.PromiseOfPower ( promiseOfPower, PromiseOfPower (..),) where

import Arkham.Helpers.ChaosBag
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype PromiseOfPower = PromiseOfPower SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

promiseOfPower :: SkillCard PromiseOfPower
promiseOfPower =
  skill PromiseOfPower Cards.promiseOfPower

instance RunMessage PromiseOfPower where
  runMessage msg (PromiseOfPower attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      n <- getRemainingCurseTokens
      if n > 0
        then addCurseTokens 1
        else assignHorror iid attrs 2
      PromiseOfPower <$> liftRunMessage msg attrs
    _ -> PromiseOfPower <$> liftRunMessage msg attrs
