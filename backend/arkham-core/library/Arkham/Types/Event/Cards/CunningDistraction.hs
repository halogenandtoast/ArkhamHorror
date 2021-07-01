module Arkham.Types.Event.Cards.CunningDistraction where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target

newtype CunningDistraction = CunningDistraction EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cunningDistraction :: EventCard CunningDistraction
cunningDistraction = event CunningDistraction Cards.cunningDistraction

instance HasModifiersFor env CunningDistraction where
  getModifiersFor = noModifiersFor

instance HasActions env CunningDistraction where
  getActions i window (CunningDistraction attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env CunningDistraction where
  runMessage msg e@(CunningDistraction attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      locationId <- getId @LocationId iid
      enemyIds <- getSetList locationId
      e <$ unshiftMessages
        ([ EnemyEvaded iid enemyId | enemyId <- enemyIds ]
        <> [Discard (EventTarget eid)]
        )
    _ -> CunningDistraction <$> runMessage msg attrs
