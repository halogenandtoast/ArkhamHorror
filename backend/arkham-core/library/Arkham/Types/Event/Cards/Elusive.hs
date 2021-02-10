module Arkham.Types.Event.Cards.Elusive where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EventId
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype Elusive = Elusive EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elusive :: InvestigatorId -> EventId -> Elusive
elusive iid uuid = Elusive $ baseAttrs iid uuid "01050"

instance HasModifiersFor env Elusive where
  getModifiersFor = noModifiersFor

instance HasActions env Elusive where
  getActions i window (Elusive attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Elusive where
  runMessage msg e@(Elusive attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      enemyIds <- getSetList iid
      emptyLocations <- mapSet unEmptyLocationId <$> getSet ()
      revealedLocations <- mapSet unRevealedLocationId <$> getSet ()
      let
        candidateLocations =
          setToList $ emptyLocations `intersection` revealedLocations

      e <$ unshiftMessages
        ([ DisengageEnemy iid enemyId | enemyId <- enemyIds ]
        <> [ chooseOne iid [ MoveTo iid lid | lid <- candidateLocations ]
           | not (null candidateLocations)
           ]
        <> [Discard (EventTarget eventId)]
        )
    _ -> Elusive <$> runMessage msg attrs
