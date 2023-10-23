module Arkham.Agenda.Cards.TheTrueCulpritV9 (
  TheTrueCulpritV9 (..),
  theTrueCulpritV9,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue

newtype TheTrueCulpritV9 = TheTrueCulpritV9 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV9 :: AgendaCard TheTrueCulpritV9
theTrueCulpritV9 = agenda (3, A) TheTrueCulpritV9 Cards.theTrueCulpritV9 (Static 6)

instance RunMessage TheTrueCulpritV9 where
  runMessage msg a@(TheTrueCulpritV9 attrs) =
    case msg of
      AdvanceAgenda aid
        | aid == toId attrs && onSide B attrs ->
            a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheTrueCulpritV9 <$> runMessage msg attrs
