module Arkham.Skill.Cards.TakeTheInitiative
  ( takeTheInitiative
  , TakeTheInitiative(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.History
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
import Arkham.SkillType

newtype TakeTheInitiative = TakeTheInitiative SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

takeTheInitiative :: SkillCard TakeTheInitiative
takeTheInitiative = skill TakeTheInitiative Cards.takeTheInitiative

instance HasModifiersFor TakeTheInitiative where
  getModifiersFor target (TakeTheInitiative a) | isTarget a target = do
    iids <- getInvestigatorIds
    histories <- traverse (getHistory PhaseHistory) iids
    let total = sum $ map historyActionsCompleted histories
    pure $ toModifiers
      a
      [ RemoveSkillIcons $ replicate (min 3 total) SkillWild | total > 0 ]
  getModifiersFor _ _ = pure []

instance RunMessage TakeTheInitiative where
  runMessage msg (TakeTheInitiative attrs) =
    TakeTheInitiative <$> runMessage msg attrs
