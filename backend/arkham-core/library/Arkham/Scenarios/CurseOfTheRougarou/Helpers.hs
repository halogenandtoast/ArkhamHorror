module Arkham.Scenarios.CurseOfTheRougarou.Helpers where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Trait

bayouLocations
  :: (MonadReader env m, HasSet LocationId env [Trait])
  => m (HashSet LocationId)
bayouLocations = getSet [Bayou]

nonBayouLocations
  :: ( MonadReader env m
     , HasSet LocationId env ()
     , HasSet LocationId env [Trait]
     )
  => m (HashSet LocationId)
nonBayouLocations = difference <$> getLocationSet <*> bayouLocations

getTheRougarou
  :: (MonadReader env m, Query EnemyMatcher env) => m (Maybe EnemyId)
getTheRougarou = selectOne $ enemyIs Cards.theRougarou
