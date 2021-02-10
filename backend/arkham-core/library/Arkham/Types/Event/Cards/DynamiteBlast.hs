module Arkham.Types.Event.Cards.DynamiteBlast where

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

newtype DynamiteBlast = DynamiteBlast EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dynamiteBlast :: InvestigatorId -> EventId -> DynamiteBlast
dynamiteBlast iid uuid = DynamiteBlast $ baseAttrs iid uuid "01023"

instance HasModifiersFor env DynamiteBlast where
  getModifiersFor = noModifiersFor

instance HasActions env DynamiteBlast where
  getActions i window (DynamiteBlast attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env DynamiteBlast where
  runMessage msg e@(DynamiteBlast attrs@EventAttrs {..}) = case msg of
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
      let availableChoices = filter (not . null) choices
      e <$ unshiftMessages
        [chooseOne iid $ map Run availableChoices, Discard (EventTarget eid)]
    _ -> DynamiteBlast <$> runMessage msg attrs
