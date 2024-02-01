module Arkham.Skill.Cards.SayYourPrayers (
  sayYourPrayers,
  SayYourPrayers (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype SayYourPrayers = SayYourPrayers SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

sayYourPrayers :: SkillCard SayYourPrayers
sayYourPrayers = skill SayYourPrayers Cards.sayYourPrayers

instance RunMessage SayYourPrayers where
  runMessage msg (SayYourPrayers attrs) =
    SayYourPrayers <$> runMessage msg attrs
