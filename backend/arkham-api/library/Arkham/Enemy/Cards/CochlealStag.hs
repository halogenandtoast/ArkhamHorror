module Arkham.Enemy.Cards.CochlealStag (cochlealStag) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.SkillType

newtype CochlealStag = CochlealStag EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cochlealStag :: EnemyCard CochlealStag
cochlealStag = enemy CochlealStag Cards.cochlealStag (3, Static 5, 4) (2, 1)

instance HasModifiersFor CochlealStag where
  getModifiersFor (CochlealStag a) = do
    phase <- getPhase
    when (phase == #mythos) do
      modifySelect
        a
        (InvestigatorAt $ orConnected_ (locationWithEnemy a))
        [SkillModifier sType (-2) | sType <- allSkills]
