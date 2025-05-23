module Arkham.Skill.Cards.DoubleOrNothing (doubleOrNothing) where

import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

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
  runMessage msg (DoubleOrNothing attrs) = DoubleOrNothing <$> runMessage msg attrs
