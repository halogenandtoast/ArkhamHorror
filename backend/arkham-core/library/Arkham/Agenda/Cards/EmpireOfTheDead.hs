module Arkham.Agenda.Cards.EmpireOfTheDead
  ( EmpireOfTheDead
  , empireOfTheDead
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Attrs
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype EmpireOfTheDead = EmpireOfTheDead AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

empireOfTheDead :: AgendaCard EmpireOfTheDead
empireOfTheDead = agenda (1, A) EmpireOfTheDead Cards.empireOfTheDead (Static 12)

instance AgendaRunner env => RunMessage env EmpireOfTheDead where
  runMessage msg a@(EmpireOfTheDead attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> EmpireOfTheDead <$> runMessage msg attrs
