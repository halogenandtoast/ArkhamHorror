module Arkham.Agenda.Cards.TemperanceXIV
  ( TemperanceXIV(..)
  , temperanceXIV
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TemperanceXIV = TemperanceXIV AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temperanceXIV :: AgendaCard TemperanceXIV
temperanceXIV = agenda (1, A) TemperanceXIV Cards.temperanceXIV (Static 8)

instance RunMessage TemperanceXIV where
  runMessage msg a@(TemperanceXIV attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> TemperanceXIV <$> runMessage msg attrs
