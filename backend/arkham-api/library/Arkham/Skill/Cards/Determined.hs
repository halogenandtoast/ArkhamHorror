module Arkham.Skill.Cards.Determined (determined) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Determined = Determined SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

determined :: SkillCard Determined
determined = skill Determined Cards.determined

instance RunMessage Determined where
  runMessage msg (Determined attrs) = runQueueT $ case msg of
    _ -> Determined <$> liftRunMessage msg attrs
