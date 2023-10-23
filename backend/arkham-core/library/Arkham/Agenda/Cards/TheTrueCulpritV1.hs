module Arkham.Agenda.Cards.TheTrueCulpritV1 (
  TheTrueCulpritV1 (..),
  theTrueCulpritV1,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue

newtype TheTrueCulpritV1 = TheTrueCulpritV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV1 :: AgendaCard TheTrueCulpritV1
theTrueCulpritV1 = agenda (3, A) TheTrueCulpritV1 Cards.theTrueCulpritV1 (Static 6)

instance RunMessage TheTrueCulpritV1 where
  runMessage msg a@(TheTrueCulpritV1 attrs) =
    case msg of
      AdvanceAgenda aid
        | aid == toId attrs && onSide B attrs ->
            a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheTrueCulpritV1 <$> runMessage msg attrs
