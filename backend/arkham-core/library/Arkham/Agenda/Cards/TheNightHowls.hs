module Arkham.Agenda.Cards.TheNightHowls
  ( TheNightHowls(..)
  , theNightHowls
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheNightHowls = TheNightHowls AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNightHowls :: AgendaCard TheNightHowls
theNightHowls = agenda (2, A) TheNightHowls Cards.theNightHowls (Static 12)

instance RunMessage TheNightHowls where
  runMessage msg a@(TheNightHowls attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> TheNightHowls <$> runMessage msg attrs
