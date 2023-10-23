module Arkham.Agenda.Cards.TheTrueCulpritV4 (
  TheTrueCulpritV4 (..),
  theTrueCulpritV4,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue

newtype TheTrueCulpritV4 = TheTrueCulpritV4 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV4 :: AgendaCard TheTrueCulpritV4
theTrueCulpritV4 = agenda (3, A) TheTrueCulpritV4 Cards.theTrueCulpritV4 (Static 14)

instance RunMessage TheTrueCulpritV4 where
  runMessage msg a@(TheTrueCulpritV4 attrs) =
    case msg of
      AdvanceAgenda aid
        | aid == toId attrs && onSide B attrs ->
            a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheTrueCulpritV4 <$> runMessage msg attrs
