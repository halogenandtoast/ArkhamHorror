module Arkham.Event.Cards.CunningDistraction where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Message
import Arkham.Target

newtype CunningDistraction = CunningDistraction EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cunningDistraction :: EventCard CunningDistraction
cunningDistraction = event CunningDistraction Cards.cunningDistraction

instance EventRunner env => RunMessage CunningDistraction where
  runMessage msg e@(CunningDistraction attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      locationId <- getId @LocationId iid
      enemyIds <- getSetList locationId
      e <$ pushAll
        ([ EnemyEvaded iid enemyId | enemyId <- enemyIds ]
        <> [Discard (EventTarget eid)]
        )
    _ -> CunningDistraction <$> runMessage msg attrs
