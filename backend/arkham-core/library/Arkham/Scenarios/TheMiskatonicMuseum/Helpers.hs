module Arkham.Scenarios.TheMiskatonicMuseum.Helpers where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Id
import Arkham.Matcher

getHuntingHorror :: Query EnemyMatcher m => m (Maybe EnemyId)
getHuntingHorror = getHuntingHorrorWith AnyEnemy

getHuntingHorrorWith
  :: Query EnemyMatcher m => EnemyMatcher -> m (Maybe EnemyId)
getHuntingHorrorWith matcher =
  selectOne $ enemyIs Cards.huntingHorror <> matcher
