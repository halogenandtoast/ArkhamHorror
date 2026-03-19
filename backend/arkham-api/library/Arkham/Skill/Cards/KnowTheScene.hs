module Arkham.Skill.Cards.KnowTheScene (knowTheScene) where

import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTestAction)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype KnowTheScene = KnowTheScene SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knowTheScene :: SkillCard KnowTheScene
knowTheScene = skill KnowTheScene Cards.knowTheScene

instance HasModifiersFor KnowTheScene where
  getModifiersFor (KnowTheScene a) = maybeModified_ a a.cardId do
    mAction <- lift getSkillTestAction
    guard $ mAction `elem` [Just #investigate, Just #parley]
    pure [AddSkillIcons [#wild, #wild]]

instance RunMessage KnowTheScene where
  runMessage msg (KnowTheScene attrs) = KnowTheScene <$> runMessage msg attrs
