module Arkham.Scenarios.TheMiskatonicMuseum.Helpers where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher
import Arkham.Store

getHuntingHorror :: (HasGame m, Store m Card) => m (Maybe EnemyId)
getHuntingHorror = getHuntingHorrorWith AnyEnemy

getHuntingHorrorWith
  :: (HasGame m, Store m Card) => EnemyMatcher -> m (Maybe EnemyId)
getHuntingHorrorWith matcher =
  selectOne $ enemyIs Cards.huntingHorror <> matcher

getRestrictedHall :: (HasGame m, Store m Card) => m LocationId
getRestrictedHall =
  selectJust $ LocationWithFullTitle "Exhibit Hall" "Restricted Hall"
