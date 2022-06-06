module Arkham.Scenarios.TheMiskatonicMuseum.Helpers where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher

getHuntingHorror :: GameT (Maybe EnemyId)
getHuntingHorror = getHuntingHorrorWith AnyEnemy

getHuntingHorrorWith :: EnemyMatcher -> GameT (Maybe EnemyId)
getHuntingHorrorWith matcher =
  selectOne $ enemyIs Cards.huntingHorror <> matcher
