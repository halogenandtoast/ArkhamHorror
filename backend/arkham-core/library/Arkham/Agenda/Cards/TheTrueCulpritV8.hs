module Arkham.Agenda.Cards.TheTrueCulpritV8
  ( TheTrueCulpritV8(..)
  , theTrueCulpritV8
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheTrueCulpritV8 = TheTrueCulpritV8 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV8 :: AgendaCard TheTrueCulpritV8
theTrueCulpritV8 = agenda (3, A) TheTrueCulpritV8 Cards.theTrueCulpritV8 (Static 6)

instance RunMessage TheTrueCulpritV8 where
  runMessage msg a@(TheTrueCulpritV8 attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheTrueCulpritV8 <$> runMessage msg attrs
