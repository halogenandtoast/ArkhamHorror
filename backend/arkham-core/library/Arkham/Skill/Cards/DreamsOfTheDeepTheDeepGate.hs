module Arkham.Skill.Cards.DreamsOfTheDeepTheDeepGate
  ( dreamsOfTheDeepTheDeepGate
  , DreamsOfTheDeepTheDeepGate(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype DreamsOfTheDeepTheDeepGate = DreamsOfTheDeepTheDeepGate SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamsOfTheDeepTheDeepGate :: SkillCard DreamsOfTheDeepTheDeepGate
dreamsOfTheDeepTheDeepGate =
  skill DreamsOfTheDeepTheDeepGate Cards.dreamsOfTheDeepTheDeepGate

instance RunMessage DreamsOfTheDeepTheDeepGate where
  runMessage msg (DreamsOfTheDeepTheDeepGate attrs) = DreamsOfTheDeepTheDeepGate <$> runMessage msg attrs
