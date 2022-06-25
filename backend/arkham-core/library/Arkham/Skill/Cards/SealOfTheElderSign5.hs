module Arkham.Skill.Cards.SealOfTheElderSign5
  ( sealOfTheElderSign5
  , SealOfTheElderSign5(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
import Arkham.Strategy
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Helpers.Modifiers

newtype SealOfTheElderSign5 = SealOfTheElderSign5 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor SealOfTheElderSign5 where
  getModifiersFor (SkillTestSource{}) (InvestigatorTarget _) (SealOfTheElderSign5 attrs) =
    pure $ toModifiers
      attrs
      [DoNotDrawChaosTokensForSkillChecks, TreatRevealedTokenAs ElderSign]
  getModifiersFor _ _ _ = pure []

sealOfTheElderSign5 :: SkillCard SealOfTheElderSign5
sealOfTheElderSign5 = skillWith SealOfTheElderSign5 Cards.sealOfTheElderSign5 (afterPlayL .~ RemoveThisFromGame)

instance RunMessage SealOfTheElderSign5 where
  runMessage msg (SealOfTheElderSign5 attrs) =
    SealOfTheElderSign5 <$> runMessage msg attrs
