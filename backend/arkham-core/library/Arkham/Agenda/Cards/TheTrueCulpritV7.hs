module Arkham.Agenda.Cards.TheTrueCulpritV7
  ( TheTrueCulpritV7(..)
  , theTrueCulpritV7
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheTrueCulpritV7 = TheTrueCulpritV7 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV7 :: AgendaCard TheTrueCulpritV7
theTrueCulpritV7 = agenda (3, A) TheTrueCulpritV7 Cards.theTrueCulpritV7 (Static 8)

instance RunMessage TheTrueCulpritV7 where
  runMessage msg a@(TheTrueCulpritV7 attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheTrueCulpritV7 <$> runMessage msg attrs
