module Arkham.Types.Event.Cards.Elusive where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target

newtype Elusive = Elusive EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elusive :: EventCard Elusive
elusive = event Elusive Cards.elusive

instance HasModifiersFor env Elusive where
  getModifiersFor = noModifiersFor

instance HasActions env Elusive where
  getActions i window (Elusive attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Elusive where
  runMessage msg e@(Elusive attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
      enemyIds <- getSetList iid
      emptyLocations <- mapSet unEmptyLocationId <$> getSet ()
      revealedLocations <- mapSet unRevealedLocationId <$> getSet ()
      let
        candidateLocations =
          setToList $ emptyLocations `intersection` revealedLocations

      e <$ pushAll
        ([ DisengageEnemy iid enemyId | enemyId <- enemyIds ]
        <> [ chooseOne iid [ MoveTo iid lid | lid <- candidateLocations ]
           | notNull candidateLocations
           ]
        <> [Discard (EventTarget eventId)]
        )
    _ -> Elusive <$> runMessage msg attrs
