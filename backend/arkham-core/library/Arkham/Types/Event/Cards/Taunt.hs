module Arkham.Types.Event.Cards.Taunt
  ( taunt
  , Taunt(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Message

newtype Taunt = Taunt EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

taunt :: EventCard Taunt
taunt = event Taunt Cards.taunt

instance HasActions env Taunt where
  getActions iid window (Taunt attrs) = getActions iid window attrs

instance HasModifiersFor env Taunt where
  getModifiersFor = noModifiersFor

instance (EventRunner env) => RunMessage env Taunt where
  runMessage msg e@(Taunt attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      lid <- getId @LocationId iid
      enemyIds <- getSetList lid
      e <$ unshiftMessage
        (chooseSome iid [ EngageEnemy iid enemyId False | enemyId <- enemyIds ])
    _ -> Taunt <$> runMessage msg attrs
