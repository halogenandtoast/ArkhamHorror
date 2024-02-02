module Arkham.Event.Cards.QuantumFlux (
  quantumFlux,
  QuantumFlux (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner

newtype QuantumFlux = QuantumFlux EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

quantumFlux :: EventCard QuantumFlux
quantumFlux = eventWith QuantumFlux Cards.quantumFlux $ afterPlayL .~ RemoveThisFromGame

instance RunMessage QuantumFlux where
  runMessage msg e@(QuantumFlux attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      drawing <- drawCards iid attrs 1
      pushAll
        [ ShuffleDiscardBackIn iid
        , drawing
        ]
      pure e
    _ -> QuantumFlux <$> runMessage msg attrs
