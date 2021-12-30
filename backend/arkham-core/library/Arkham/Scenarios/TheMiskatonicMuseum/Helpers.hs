module Arkham.Scenarios.TheMiskatonicMuseum.Helpers where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher

getHuntingHorror
  :: (MonadReader env m, Query EnemyMatcher env) => m (Maybe EnemyId)
getHuntingHorror = getHuntingHorrorWith AnyEnemy

getHuntingHorrorWith
  :: (MonadReader env m, Query EnemyMatcher env)
  => EnemyMatcher
  -> m (Maybe EnemyId)
getHuntingHorrorWith matcher =
  selectOne $ enemyIs Cards.huntingHorror <> matcher
