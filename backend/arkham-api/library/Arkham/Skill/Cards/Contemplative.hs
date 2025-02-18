module Arkham.Skill.Cards.Contemplative (contemplative) where

import Arkham.Helpers.SkillTest
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Contemplative = Contemplative SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

contemplative :: SkillCard Contemplative
contemplative = skill Contemplative Cards.contemplative

instance RunMessage Contemplative where
  runMessage msg s@(Contemplative attrs) = runQueueT $ case msg of
    PassedSkillTest _iid _ _ (isTarget attrs -> True) _ _ -> do
      getSkillTestInvestigator >>= traverse_ \iid -> do
        discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure s
    _ -> Contemplative <$> liftRunMessage msg attrs
