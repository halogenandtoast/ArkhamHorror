module Arkham.Skill.Cards.WellDressed (wellDressed, WellDressed (..)) where

import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (isParley)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype WellDressed = WellDressed SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellDressed :: SkillCard WellDressed
wellDressed = skill WellDressed Cards.wellDressed

instance HasModifiersFor WellDressed where
  getModifiersFor (WellDressed attrs) = maybeModified_ attrs attrs.cardId do
    liftGuardM isParley
    pure [AddSkillIcons [#wild, #wild, #wild]]

instance RunMessage WellDressed where
  runMessage msg (WellDressed attrs) = runQueueT $ case msg of
    _ -> WellDressed <$> liftRunMessage msg attrs
