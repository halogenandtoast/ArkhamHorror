module Arkham.Skill.Cards.Gumption1 (gumption1, Gumption1 (..)) where

import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Gumption1 = Gumption1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gumption1 :: SkillCard Gumption1
gumption1 = skill Gumption1 Cards.gumption1

instance HasModifiersFor Gumption1 where
  getModifiersFor (Gumption1 a) =
    getSkillTestId >>= \case
      Nothing -> pure mempty
      Just sid -> modified_ a sid [Difficulty (-2)]

instance RunMessage Gumption1 where
  runMessage msg (Gumption1 attrs) = runQueueT $ case msg of
    _ -> Gumption1 <$> liftRunMessage msg attrs
