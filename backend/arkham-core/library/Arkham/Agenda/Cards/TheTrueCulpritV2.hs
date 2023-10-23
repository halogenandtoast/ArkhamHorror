module Arkham.Agenda.Cards.TheTrueCulpritV2 (
  TheTrueCulpritV2 (..),
  theTrueCulpritV2,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue

newtype TheTrueCulpritV2 = TheTrueCulpritV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV2 :: AgendaCard TheTrueCulpritV2
theTrueCulpritV2 = agenda (3, A) TheTrueCulpritV2 Cards.theTrueCulpritV2 (Static 8)

instance RunMessage TheTrueCulpritV2 where
  runMessage msg a@(TheTrueCulpritV2 attrs) =
    case msg of
      AdvanceAgenda aid
        | aid == toId attrs && onSide B attrs ->
            a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheTrueCulpritV2 <$> runMessage msg attrs
