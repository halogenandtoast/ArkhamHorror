module Arkham.Scenarios.TheMiskatonicMuseum.Helpers where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher

getHuntingHorror :: (Monad m, HasGame m) => m (Maybe EnemyId)
getHuntingHorror = getHuntingHorrorWith AnyEnemy

getHuntingHorrorWith :: (Monad m, HasGame m) => EnemyMatcher -> m (Maybe EnemyId)
getHuntingHorrorWith matcher =
  selectOne $ enemyIs Cards.huntingHorror <> matcher
