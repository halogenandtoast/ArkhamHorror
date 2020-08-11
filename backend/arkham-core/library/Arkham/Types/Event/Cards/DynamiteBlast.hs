module Arkham.Types.Event.Cards.DynamiteBlast where

import Arkham.Types.Classes
import Arkham.Types.GameRunner
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Source
import ClassyPrelude
import qualified Data.HashSet as HashSet

dynamiteBlast
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
dynamiteBlast iid = do
  currentLocationId <- asks (getId @LocationId iid)
  connectedLocationIds <-
    HashSet.toList . HashSet.map unConnectedLocationId <$> asks
      (getSet currentLocationId)
  choices <- for (currentLocationId : connectedLocationIds) $ \lid -> do
    enemyIds <- HashSet.toList <$> asks (getSet lid)
    investigatorIds <- HashSet.toList <$> asks (getSet @InvestigatorId lid)
    pure
      $ map (\eid -> EnemyDamage eid iid (EventSource "01023") 3) enemyIds
      <> map
           (\iid' -> InvestigatorAssignDamage iid' (EventSource "01023") 3 0)
           investigatorIds
  unshiftMessage (Ask iid $ ChooseOne $ concat choices)
