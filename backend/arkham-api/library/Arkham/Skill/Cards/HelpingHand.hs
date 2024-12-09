module Arkham.Skill.Cards.HelpingHand (helpingHand, HelpingHand (..)) where

import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype HelpingHand = HelpingHand SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

helpingHand :: SkillCard HelpingHand
helpingHand = skill HelpingHand Cards.helpingHand

instance HasModifiersFor HelpingHand where
  getModifiersFor (HelpingHand attrs) = do
    getSkillTest >>= \case
      Nothing -> pure mempty
      Just st -> do
        (<>)
          <$> modifyEach attrs (concat $ toList $ st.committedCards) [DoubleSkillIcons]
          <*> modifySelect attrs (NotSkill $ SkillWithId attrs.id) [DoubleSkillIcons]

instance RunMessage HelpingHand where
  runMessage msg (HelpingHand attrs) = runQueueT $ case msg of
    _ -> HelpingHand <$> liftRunMessage msg attrs
