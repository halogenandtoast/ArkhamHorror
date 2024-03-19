module Arkham.Agenda.Cards.TheShapeOfChaos
  ( TheShapeOfChaos(..)
  , theShapeOfChaos
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheShapeOfChaos = TheShapeOfChaos AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theShapeOfChaos :: AgendaCard TheShapeOfChaos
theShapeOfChaos = agenda (2, A) TheShapeOfChaos Cards.theShapeOfChaos (Static 5)

instance RunMessage TheShapeOfChaos where
  runMessage msg a@(TheShapeOfChaos attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheShapeOfChaos <$> runMessage msg attrs
