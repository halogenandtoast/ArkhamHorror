module Arkham.Event.Cards.Elusive where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Message
import Arkham.Target

newtype Elusive = Elusive EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elusive :: EventCard Elusive
elusive = event Elusive Cards.elusive

instance EventRunner env => RunMessage Elusive where
  runMessage msg e@(Elusive attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      enemyIds <- getSetList iid
      emptyLocations <- mapSet unEmptyLocationId <$> getSet ()
      revealedLocations <- mapSet unRevealedLocationId <$> getSet ()
      let
        candidateLocations =
          setToList $ emptyLocations `intersection` revealedLocations

      e <$ pushAll
        ([ DisengageEnemy iid enemyId | enemyId <- enemyIds ]
        <> [ chooseOne
               iid
               [ MoveTo (toSource attrs) iid lid | lid <- candidateLocations ]
           | notNull candidateLocations
           ]
        <> map EnemyCheckEngagement enemyIds
        <> [Discard (EventTarget eventId)]
        )
    _ -> Elusive <$> runMessage msg attrs
