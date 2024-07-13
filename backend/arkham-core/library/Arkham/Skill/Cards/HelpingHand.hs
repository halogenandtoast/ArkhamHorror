module Arkham.Skill.Cards.HelpingHand (helpingHand, HelpingHand (..)) where

import Arkham.Helpers.Modifiers
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype HelpingHand = HelpingHand SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

helpingHand :: SkillCard HelpingHand
helpingHand = skill HelpingHand Cards.helpingHand

instance HasModifiersFor HelpingHand where
  getModifiersFor (CardIdTarget cid) (HelpingHand attrs) | attrs.cardId /= cid = do
    modified attrs [DoubleSkillIcons]
  getModifiersFor (SkillTarget s) (HelpingHand attrs) | attrs.id /= s = do
    modified attrs [DoubleSkillIcons]
  getModifiersFor _ _ = pure []

instance RunMessage HelpingHand where
  runMessage msg (HelpingHand attrs) = runQueueT $ case msg of
    _ -> HelpingHand <$> liftRunMessage msg attrs
