module Arkham.Skill.Cards.Hardboiled (hardboiled) where

import {-# SOURCE #-} Arkham.GameEnv (getHistoryField)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.History
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Data.List qualified as L

newtype Hardboiled = Hardboiled SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hardboiled :: SkillCard Hardboiled
hardboiled = skill Hardboiled Cards.hardboiled

instance HasModifiersFor Hardboiled where
  getModifiersFor (Hardboiled attrs) = do
    n <- getHistoryField #round attrs.controller HistorySuccessfulAttacks
    when (n > 0) do
      modified_ attrs attrs.cardId [AddSkillIcons $ concat $ L.replicate n [#combat, #wild]]

instance RunMessage Hardboiled where
  runMessage msg (Hardboiled attrs) = Hardboiled <$> runMessage msg attrs
