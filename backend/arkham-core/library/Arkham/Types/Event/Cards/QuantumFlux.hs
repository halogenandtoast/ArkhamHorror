module Arkham.Types.Event.Cards.QuantumFlux
  ( quantumFlux
  , QuantumFlux(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message

newtype QuantumFlux = QuantumFlux EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quantumFlux :: EventCard QuantumFlux
quantumFlux = event QuantumFlux Cards.quantumFlux

instance EventRunner env => RunMessage env QuantumFlux where
  runMessage msg e@(QuantumFlux attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ ShuffleDiscardBackIn iid
        , DrawCards iid 1 False
        , RemoveFromGame (toTarget attrs)
        ]
    _ -> QuantumFlux <$> runMessage msg attrs
