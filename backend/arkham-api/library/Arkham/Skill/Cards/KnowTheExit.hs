module Arkham.Skill.Cards.KnowTheExit (knowTheExit) where

import Arkham.Helpers.Modifiers (maybeModified_, ModifierType(..))
import Arkham.Helpers.SkillTest (getSkillTestAction)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype KnowTheExit = KnowTheExit SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knowTheExit :: SkillCard KnowTheExit
knowTheExit = skill KnowTheExit Cards.knowTheExit

instance HasModifiersFor KnowTheExit where
  getModifiersFor (KnowTheExit a) = maybeModified_ a a.cardId do
    mAction <- lift getSkillTestAction
    guard $ mAction `elem` [Just #fight, Just #evade]
    pure [AddSkillIcons [#wild, #wild]]

instance RunMessage KnowTheExit where
  runMessage msg (KnowTheExit attrs) = KnowTheExit <$> runMessage msg attrs
