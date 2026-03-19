module Arkham.Skill.Cards.KnowTheLine (knowTheLine) where

import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTestSource)
import Arkham.Helpers.Source (sourceMatches)
import Arkham.Matcher (SourceMatcher (ScenarioCardSource))
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype KnowTheLine = KnowTheLine SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knowTheLine :: SkillCard KnowTheLine
knowTheLine = skill KnowTheLine Cards.knowTheLine

instance HasModifiersFor KnowTheLine where
  getModifiersFor (KnowTheLine a) = maybeModified_ a a.cardId do
    t <- MaybeT getSkillTestSource
    liftGuardM $ sourceMatches t ScenarioCardSource
    pure [AddSkillIcons [#wild, #wild]]

instance RunMessage KnowTheLine where
  runMessage msg (KnowTheLine attrs) = KnowTheLine <$> runMessage msg attrs
