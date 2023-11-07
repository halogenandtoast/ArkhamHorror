module Arkham.Skill.Cards.PromiseOfPower (
  promiseOfPower,
  PromiseOfPower (..),
)
where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Helpers.ChaosBag
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype PromiseOfPower = PromiseOfPower SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

promiseOfPower :: SkillCard PromiseOfPower
promiseOfPower =
  skill PromiseOfPower Cards.promiseOfPower

instance RunMessage PromiseOfPower where
  runMessage msg s@(PromiseOfPower attrs) = case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      n <- getRemainingCurseTokens
      if n > 0
        then push $ AddChaosToken CurseToken
        else push $ assignHorror iid attrs 2
      pure s
    _ -> PromiseOfPower <$> runMessage msg attrs
