module Arkham.Skill.Cards.Unrelenting1
  ( unrelenting1
  , Unrelenting1(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Unrelenting1 = Unrelenting1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unrelenting1 :: SkillCard Unrelenting1
unrelenting1 =
  skill Unrelenting1 Cards.unrelenting1

instance RunMessage Unrelenting1 where
  runMessage msg (Unrelenting1 attrs) = Unrelenting1 <$> runMessage msg attrs
