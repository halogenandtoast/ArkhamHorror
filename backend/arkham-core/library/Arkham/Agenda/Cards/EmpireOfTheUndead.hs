module Arkham.Agenda.Cards.EmpireOfTheUndead
  ( EmpireOfTheUndead
  , empireOfTheUndead
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Attrs
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype EmpireOfTheUndead = EmpireOfTheUndead AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

empireOfTheUndead :: AgendaCard EmpireOfTheUndead
empireOfTheUndead = agenda (1, A) EmpireOfTheUndead Cards.empireOfTheUndead (Static 12)

instance AgendaRunner env => RunMessage env EmpireOfTheUndead where
  runMessage msg a@(EmpireOfTheUndead attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> EmpireOfTheUndead <$> runMessage msg attrs
