module Arkham.Event.Cards.DarkMemory where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype DarkMemory = DarkMemory EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkMemory :: EventCard DarkMemory
darkMemory = event DarkMemory Cards.darkMemory

instance HasAbilities DarkMemory where
  getAbilities (DarkMemory x) =
    [ restrictedAbility x 1 InYourHand
        $ ForcedAbility
        $ TurnEnds Timing.When You
    ]

instance RunMessage DarkMemory where
  runMessage msg e@(DarkMemory attrs) = case msg of
    InHand iid' (UseCardAbility iid (isSource attrs -> True) 1 _ _) | iid' == iid -> do
      pushAll
        [ RevealInHand $ toCardId attrs
        , assignHorror iid (CardSource $ toCard attrs) 2
        ]
      pure e
    InvestigatorPlayEvent _ eid _ _ _ | eid == toId attrs -> do
      pushAll [PlaceDoomOnAgenda, AdvanceAgendaIfThresholdSatisfied]
      pure e
    _ -> DarkMemory <$> runMessage msg attrs
