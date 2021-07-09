module Arkham.Types.Event.Cards.SneakAttack where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

newtype SneakAttack = SneakAttack EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sneakAttack :: EventCard SneakAttack
sneakAttack = event SneakAttack Cards.sneakAttack

instance HasModifiersFor env SneakAttack where
  getModifiersFor = noModifiersFor

instance HasActions env SneakAttack where
  getActions i window (SneakAttack attrs) = getActions i window attrs

instance EventRunner env => RunMessage env SneakAttack where
  runMessage msg e@(SneakAttack attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
      lid <- getId @LocationId iid
      enemyIds <- map unExhaustedEnemyId <$> getSetList lid
      e <$ pushAll
        ([ EnemyDamage enemyId iid (EventSource eventId) 2
         | enemyId <- enemyIds
         ]
        <> [Discard (EventTarget eventId)]
        )
    _ -> SneakAttack <$> runMessage msg attrs
