module Arkham.Types.Enemy.Cards.BalefulReveler
  ( balefulReveler
  , BalefulReveler(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Id
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Control.Monad.Extra (findM)

newtype BalefulReveler = BalefulReveler EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

balefulReveler :: EnemyCard BalefulReveler
balefulReveler =
  enemy BalefulReveler Cards.balefulReveler (4, PerPlayer 5, 3) (2, 2)

instance HasModifiersFor env BalefulReveler

instance EnemyAttrsHasActions env => HasActions env BalefulReveler where
  getActions i window (BalefulReveler attrs) = getActions i window attrs

getCounterClockwiseLocations
  :: ( MonadReader env m
     , HasSet LocationId env ()
     , HasSet ConnectedLocationId env LocationId
     )
  => LocationId
  -> m [LocationId]
getCounterClockwiseLocations end = do
  lids <- getSetList @LocationId ()
  flippedMap :: HashMap LocationId LocationId <-
    mapFromList
    . concat
    <$> traverse
          (\lid -> map ((, lid) . unConnectedLocationId) <$> getSetList lid)
          lids
  pure $ buildList (lookup end flippedMap) flippedMap
 where
  buildList Nothing _ = []
  buildList (Just current) _ | current == end = [end]
  buildList (Just current) flippedMap =
    current : buildList (lookup current flippedMap) flippedMap

instance EnemyAttrsRunMessage env => RunMessage env BalefulReveler where
  runMessage msg (BalefulReveler attrs) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == toId attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      start <- getId @LocationId leadInvestigatorId
      locations <- getCounterClockwiseLocations start

      mSpawnLocation <- findM (fmap null . getSet @InvestigatorId) locations

      case mSpawnLocation of
        Just spawnLocation -> BalefulReveler <$> runMessage
          msg
          (attrs & spawnAtL ?~ LocationWithId spawnLocation)
        Nothing -> error "could not find location for baleful reveler"
    _ -> BalefulReveler <$> runMessage msg attrs
