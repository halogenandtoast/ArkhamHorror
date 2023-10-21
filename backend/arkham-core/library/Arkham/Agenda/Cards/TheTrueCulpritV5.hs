module Arkham.Agenda.Cards.TheTrueCulpritV5
  ( TheTrueCulpritV5(..)
  , theTrueCulpritV5
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheTrueCulpritV5 = TheTrueCulpritV5 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV5 :: AgendaCard TheTrueCulpritV5
theTrueCulpritV5 = agenda (3, A) TheTrueCulpritV5 Cards.theTrueCulpritV5 (Static 6)

instance RunMessage TheTrueCulpritV5 where
  runMessage msg a@(TheTrueCulpritV5 attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheTrueCulpritV5 <$> runMessage msg attrs
