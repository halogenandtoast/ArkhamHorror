module Arkham.Agenda.Cards.Vengeance
  ( Vengeance(..)
  , vengeance
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype Vengeance = Vengeance AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengeance :: AgendaCard Vengeance
vengeance = agenda (7, A) Vengeance Cards.vengeance (Static 0)

instance RunMessage Vengeance where
  runMessage msg a@(Vengeance attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> Vengeance <$> runMessage msg attrs
