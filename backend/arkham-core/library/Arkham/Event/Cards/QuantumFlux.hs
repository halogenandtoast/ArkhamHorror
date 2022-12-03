module Arkham.Event.Cards.QuantumFlux
  ( quantumFlux
  , QuantumFlux(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message

newtype QuantumFlux = QuantumFlux EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quantumFlux :: EventCard QuantumFlux
quantumFlux = event QuantumFlux Cards.quantumFlux

instance RunMessage QuantumFlux where
  runMessage msg e@(QuantumFlux attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ ShuffleDiscardBackIn iid
        , drawCards iid attrs 1
        , RemoveFromGame (toTarget attrs)
        ]
    _ -> QuantumFlux <$> runMessage msg attrs
