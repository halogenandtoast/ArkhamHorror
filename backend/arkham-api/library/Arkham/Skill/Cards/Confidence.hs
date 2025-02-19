module Arkham.Skill.Cards.Confidence (confidence) where

import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Modifiers
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Confidence = Confidence SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

confidence :: SkillCard Confidence
confidence = skill Confidence Cards.confidence

instance HasModifiersFor Confidence where
  getModifiersFor (Confidence a) = maybeModified_ a a.cardId do
    cb <- lift getChaosBag
    guard $ length cb.totalRevealed > 1
    pure [AddSkillIcons $ replicate (min 3 $ length cb.totalRevealed - 1) #wild]

instance RunMessage Confidence where
  runMessage msg (Confidence attrs) = Confidence <$> runMessage msg attrs
