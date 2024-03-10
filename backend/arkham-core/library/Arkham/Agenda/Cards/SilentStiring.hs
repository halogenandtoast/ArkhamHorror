module Arkham.Agenda.Cards.SilentStiring
  ( SilentStiring(..)
  , silentStiring
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype SilentStiring = SilentStiring AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silentStiring :: AgendaCard SilentStiring
silentStiring = agenda (1, A) SilentStiring Cards.silentStiring (Static 12)

instance RunMessage SilentStiring where
  runMessage msg a@(SilentStiring attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> SilentStiring <$> runMessage msg attrs
