module Arkham.Helpers.Enemy where

import Arkham.Attack.Types
import Arkham.Classes.HasGame
import Arkham.Id
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude

enemyAttackMatches
  :: HasGame m => InvestigatorId -> EnemyAttackDetails -> Matcher.EnemyAttackMatcher -> m Bool
