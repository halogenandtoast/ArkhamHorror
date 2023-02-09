module Arkham.Event.Cards.DarkMemory where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Criteria
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Source
import Arkham.Timing qualified as Timing

newtype DarkMemory = DarkMemory EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkMemory :: EventCard DarkMemory
darkMemory = event DarkMemory Cards.darkMemory

instance HasAbilities DarkMemory where
  getAbilities (DarkMemory x) =
    [ restrictedAbility x 1 InYourHand $ ForcedAbility $ TurnEnds
        Timing.When
        You
    ]

instance RunMessage DarkMemory where
  runMessage msg e@(DarkMemory attrs@EventAttrs {..}) = case msg of
    InHand iid' (UseCardAbility iid (isSource attrs -> True) 1 _ _) | iid' == iid -> e <$ pushAll
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
        , discard attrs
        ]
    _ -> DarkMemory <$> runMessage msg attrs
