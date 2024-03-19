module Arkham.Agenda.Cards.TheEyeOfChaos
  ( TheEyeOfChaos(..)
  , theEyeOfChaos
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheEyeOfChaos = TheEyeOfChaos AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEyeOfChaos :: AgendaCard TheEyeOfChaos
theEyeOfChaos = agenda (1, A) TheEyeOfChaos Cards.theEyeOfChaos (Static 7)

instance RunMessage TheEyeOfChaos where
  runMessage msg a@(TheEyeOfChaos attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheEyeOfChaos <$> runMessage msg attrs
