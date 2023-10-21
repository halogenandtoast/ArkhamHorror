module Arkham.Agenda.Cards.TheTrueCulpritV3
  ( TheTrueCulpritV3(..)
  , theTrueCulpritV3
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheTrueCulpritV3 = TheTrueCulpritV3 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV3 :: AgendaCard TheTrueCulpritV3
theTrueCulpritV3 = agenda (3, A) TheTrueCulpritV3 Cards.theTrueCulpritV3 (Static 6)

instance RunMessage TheTrueCulpritV3 where
  runMessage msg a@(TheTrueCulpritV3 attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheTrueCulpritV3 <$> runMessage msg attrs
