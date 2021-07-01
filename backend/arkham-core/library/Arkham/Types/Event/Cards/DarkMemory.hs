module Arkham.Types.Event.Cards.DarkMemory where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

newtype DarkMemory = DarkMemory EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkMemory :: InvestigatorId -> EventId -> DarkMemory
darkMemory = event DarkMemory Cards.darkMemory

instance HasModifiersFor env DarkMemory where
  getModifiersFor = noModifiersFor

instance HasActions env DarkMemory where
  getActions i window (DarkMemory attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env DarkMemory where
  runMessage msg e@(DarkMemory attrs@EventAttrs {..}) = case msg of
    InHand ownerId (EndTurn iid) | ownerId == iid -> e <$ unshiftMessages
      [ RevealInHand $ toCardId attrs
      , InvestigatorAssignDamage
        iid
        (PlayerCardSource $ toCardId attrs)
        DamageAny
        0
        2
      ]
    InvestigatorPlayEvent _ eid _ | eid == eventId -> do
      e <$ unshiftMessages
        [ PlaceDoomOnAgenda
        , AdvanceAgendaIfThresholdSatisfied
        , Discard (EventTarget eid)
        ]
    _ -> DarkMemory <$> runMessage msg attrs
