module Arkham.Types.Event.Cards.DarkMemory where


import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype DarkMemory = DarkMemory EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkMemory :: InvestigatorId -> EventId -> DarkMemory
darkMemory iid uuid = DarkMemory $ weaknessAttrs iid uuid "01013"

instance HasModifiersFor env DarkMemory where
  getModifiersFor = noModifiersFor

instance HasActions env DarkMemory where
  getActions i window (DarkMemory attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env DarkMemory where
  runMessage msg e@(DarkMemory attrs@EventAttrs {..}) = case msg of
    InHand ownerId (EndTurn iid) | ownerId == iid -> e <$ unshiftMessages
      [ RevealInHand $ getCardId attrs
      , InvestigatorAssignDamage
        iid
        (PlayerCardSource $ getCardId attrs)
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
