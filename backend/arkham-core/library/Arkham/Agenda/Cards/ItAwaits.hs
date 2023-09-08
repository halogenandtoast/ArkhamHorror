module Arkham.Agenda.Cards.ItAwaits
  ( ItAwaits(..)
  , itAwaits
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype ItAwaits = ItAwaits AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

itAwaits :: AgendaCard ItAwaits
itAwaits = agenda (1, A) ItAwaits Cards.itAwaits (Static 6)

instance RunMessage ItAwaits where
  runMessage msg a@(ItAwaits attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> ItAwaits <$> runMessage msg attrs
