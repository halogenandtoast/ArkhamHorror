module Arkham.Event.Cards.DarkMemory where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Criteria
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype DarkMemory = DarkMemory EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkMemory :: EventCard DarkMemory
darkMemory = event DarkMemory Cards.darkMemory

instance HasAbilities DarkMemory where
  getAbilities (DarkMemory x) =
    [ restrictedAbility x 1 InYourHand $ ForcedAbility $ TurnEnds
        Timing.When
        You
    ]

instance EventRunner env => RunMessage env DarkMemory where
  runMessage msg e@(DarkMemory attrs@EventAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> e <$ pushAll
      [ RevealInHand $ toCardId attrs
      , InvestigatorAssignDamage
        iid
        (CardIdSource $ toCardId attrs)
        DamageAny
        0
        2
      ]
    InvestigatorPlayEvent _ eid _ _ _ | eid == eventId -> do
      e <$ pushAll
        [ PlaceDoomOnAgenda
        , AdvanceAgendaIfThresholdSatisfied
        , Discard (EventTarget eid)
        ]
    _ -> DarkMemory <$> runMessage msg attrs
