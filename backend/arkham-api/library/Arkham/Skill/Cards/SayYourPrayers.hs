module Arkham.Skill.Cards.SayYourPrayers (sayYourPrayers) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype SayYourPrayers = SayYourPrayers SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sayYourPrayers :: SkillCard SayYourPrayers
sayYourPrayers = skill SayYourPrayers Cards.sayYourPrayers

instance RunMessage SayYourPrayers where
  runMessage msg (SayYourPrayers attrs) = SayYourPrayers <$> runMessage msg attrs
