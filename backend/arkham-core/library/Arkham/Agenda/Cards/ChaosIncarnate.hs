module Arkham.Agenda.Cards.ChaosIncarnate
  ( ChaosIncarnate(..)
  , chaosIncarnate
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype ChaosIncarnate = ChaosIncarnate AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosIncarnate :: AgendaCard ChaosIncarnate
chaosIncarnate = agenda (3, A) ChaosIncarnate Cards.chaosIncarnate (Static 5)

instance RunMessage ChaosIncarnate where
  runMessage msg a@(ChaosIncarnate attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> ChaosIncarnate <$> runMessage msg attrs
