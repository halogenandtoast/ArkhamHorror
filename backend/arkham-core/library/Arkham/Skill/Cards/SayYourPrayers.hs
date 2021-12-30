module Arkham.Skill.Cards.SayYourPrayers
  ( sayYourPrayers
  , SayYourPrayers(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Classes
import Arkham.Skill.Attrs
import Arkham.Skill.Runner

newtype SayYourPrayers = SayYourPrayers SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sayYourPrayers :: SkillCard SayYourPrayers
sayYourPrayers = skill SayYourPrayers Cards.sayYourPrayers

instance SkillRunner env => RunMessage env SayYourPrayers where
  runMessage msg (SayYourPrayers attrs) =
    SayYourPrayers <$> runMessage msg attrs
