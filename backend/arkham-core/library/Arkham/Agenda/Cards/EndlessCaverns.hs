module Arkham.Agenda.Cards.EndlessCaverns
  ( EndlessCaverns(..)
  , endlessCaverns
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype EndlessCaverns = EndlessCaverns AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessCaverns :: AgendaCard EndlessCaverns
endlessCaverns = agenda (3, A) EndlessCaverns Cards.endlessCaverns (Static 4)

instance RunMessage EndlessCaverns where
  runMessage msg a@(EndlessCaverns attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> EndlessCaverns <$> runMessage msg attrs
