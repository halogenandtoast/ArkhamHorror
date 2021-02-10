module Arkham.Types.Event.Cards.DynamiteBlast2 where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype DynamiteBlast2 = DynamiteBlast2 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dynamiteBlast2 :: InvestigatorId -> EventId -> DynamiteBlast2
dynamiteBlast2 iid uuid = DynamiteBlast2 $ baseAttrs iid uuid "01023"

instance HasModifiersFor env DynamiteBlast2 where
  getModifiersFor = noModifiersFor

instance HasActions env DynamiteBlast2 where
  getActions i window (DynamiteBlast2 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env DynamiteBlast2 where
  -- TODO: Does not provoke attacks of opportunity
  runMessage msg e@(DynamiteBlast2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      currentLocationId <- getId @LocationId iid
      connectedLocationIds <- map unConnectedLocationId
        <$> getSetList currentLocationId
      choices <- for (currentLocationId : connectedLocationIds) $ \lid -> do
        enemyIds <- getSetList lid
        investigatorIds <- getSetList @InvestigatorId lid
        pure
          $ map (\enid -> EnemyDamage enid iid (EventSource eid) 3) enemyIds
          <> map
               (\iid' ->
                 InvestigatorAssignDamage iid' (EventSource eid) DamageAny 3 0
               )
               investigatorIds
      e <$ unshiftMessages
        [chooseOne iid $ map Run choices, Discard (EventTarget eid)]
    _ -> DynamiteBlast2 <$> runMessage msg attrs
