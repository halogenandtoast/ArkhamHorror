module Arkham.Agenda.Cards.TheTrueCulpritV10 (
  TheTrueCulpritV10 (..),
  theTrueCulpritV10,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue

newtype TheTrueCulpritV10 = TheTrueCulpritV10 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV10 :: AgendaCard TheTrueCulpritV10
theTrueCulpritV10 = agenda (3, A) TheTrueCulpritV10 Cards.theTrueCulpritV10 (Static 12)

instance RunMessage TheTrueCulpritV10 where
  runMessage msg a@(TheTrueCulpritV10 attrs) =
    case msg of
      AdvanceAgenda aid
        | aid == toId attrs && onSide B attrs ->
            a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheTrueCulpritV10 <$> runMessage msg attrs
