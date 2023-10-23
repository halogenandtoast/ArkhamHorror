module Arkham.Agenda.Cards.TheTrueCulpritV6 (
  TheTrueCulpritV6 (..),
  theTrueCulpritV6,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue

newtype TheTrueCulpritV6 = TheTrueCulpritV6 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV6 :: AgendaCard TheTrueCulpritV6
theTrueCulpritV6 = agenda (3, A) TheTrueCulpritV6 Cards.theTrueCulpritV6 (Static 6)

instance RunMessage TheTrueCulpritV6 where
  runMessage msg a@(TheTrueCulpritV6 attrs) =
    case msg of
      AdvanceAgenda aid
        | aid == toId attrs && onSide B attrs ->
            a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheTrueCulpritV6 <$> runMessage msg attrs
