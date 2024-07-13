module Arkham.Skill.Cards.Analysis (analysis, Analysis (..)) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Analysis = Analysis SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

analysis :: SkillCard Analysis
analysis = skill Analysis Cards.analysis

-- N.B. this card is handled in the skill test itself. Mainly because we only
-- want it to trigger once

instance RunMessage Analysis where
  runMessage msg (Analysis attrs) = runQueueT $ case msg of
    _ -> Analysis <$> liftRunMessage msg attrs
