module Arkham.Agenda.Cards.TheFinalCountdown
  ( TheFinalCountdown(..)
  , theFinalCountdown
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheFinalCountdown = TheFinalCountdown AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFinalCountdown :: AgendaCard TheFinalCountdown
theFinalCountdown = agenda (1, A) TheFinalCountdown Cards.theFinalCountdown (Static 8)

instance RunMessage TheFinalCountdown where
  runMessage msg a@(TheFinalCountdown attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheFinalCountdown <$> runMessage msg attrs
