module Arkham.Agenda.Cards.EndsAndMeans
  ( EndsAndMeans(..)
  , endsAndMeans
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype EndsAndMeans = EndsAndMeans AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endsAndMeans :: AgendaCard EndsAndMeans
endsAndMeans = agenda (2, A) EndsAndMeans Cards.endsAndMeans (Static 10)

instance RunMessage EndsAndMeans where
  runMessage msg a@(EndsAndMeans attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> EndsAndMeans <$> runMessage msg attrs
