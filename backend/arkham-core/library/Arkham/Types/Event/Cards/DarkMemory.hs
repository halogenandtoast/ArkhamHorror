module Arkham.Types.Event.Cards.DarkMemory where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

newtype DarkMemory = DarkMemory EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkMemory :: EventCard DarkMemory
darkMemory = event DarkMemory Cards.darkMemory

instance EventRunner env => RunMessage env DarkMemory where
  runMessage msg e@(DarkMemory attrs@EventAttrs {..}) = case msg of
    InHand ownerId (EndTurn iid) | ownerId == iid -> e <$ pushAll
      [ RevealInHand $ toCardId attrs
      , InvestigatorAssignDamage
        iid
        (PlayerCardSource $ toCardId attrs)
        DamageAny
        0
        2
      ]
    InvestigatorPlayEvent _ eid _ _ | eid == eventId -> do
      e <$ pushAll
        [ PlaceDoomOnAgenda
        , AdvanceAgendaIfThresholdSatisfied
        , Discard (EventTarget eid)
        ]
    _ -> DarkMemory <$> runMessage msg attrs
