module Arkham.Event.Events.QuantumFlux (quantumFlux) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Strategy

newtype QuantumFlux = QuantumFlux EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quantumFlux :: EventCard QuantumFlux
quantumFlux = eventWith QuantumFlux Cards.quantumFlux $ afterPlayL .~ RemoveThisFromGame

instance RunMessage QuantumFlux where
  runMessage msg e@(QuantumFlux attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      shuffleDiscardBackIn iid
      drawCards iid attrs 1
      pure e
    _ -> QuantumFlux <$> liftRunMessage msg attrs
