module Arkham.Skill.Cards.DoubleOrNothing (
  doubleOrNothing,
  DoubleOrNothing (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype DoubleOrNothing = DoubleOrNothing SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doubleOrNothing :: SkillCard DoubleOrNothing
doubleOrNothing = skill DoubleOrNothing Cards.doubleOrNothing

instance HasModifiersFor DoubleOrNothing where
  getModifiersFor (DoubleOrNothing attrs) =
    getSkillTestId >>= \case
      Nothing -> pure mempty
      Just sid -> modified_ attrs (SkillTestTarget sid) [DoubleDifficulty, DoubleSuccess]

instance RunMessage DoubleOrNothing where
  runMessage msg (DoubleOrNothing attrs) =
    DoubleOrNothing <$> runMessage msg attrs
