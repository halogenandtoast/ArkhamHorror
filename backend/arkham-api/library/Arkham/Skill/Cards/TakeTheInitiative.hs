module Arkham.Skill.Cards.TakeTheInitiative (takeTheInitiative) where

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.History
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillType

newtype TakeTheInitiative = TakeTheInitiative SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

takeTheInitiative :: SkillCard TakeTheInitiative
takeTheInitiative = skill TakeTheInitiative Cards.takeTheInitiative

instance HasModifiersFor TakeTheInitiative where
  getModifiersFor (TakeTheInitiative a) = do
    -- we want to include investigators that were eliminated
    histories <- traverse (getHistory PhaseHistory) =<< select Anyone
    let total = sum $ map historyActionsCompleted histories
    modifySelf a.cardId [RemoveSkillIcons $ replicate (min 3 total) WildIcon | total > 0]

instance RunMessage TakeTheInitiative where
  runMessage msg (TakeTheInitiative attrs) =
    TakeTheInitiative <$> runMessage msg attrs
