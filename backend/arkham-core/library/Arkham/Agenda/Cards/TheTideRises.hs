module Arkham.Agenda.Cards.TheTideRises
  ( TheTideRises
  , theTideRises
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheTideRises = TheTideRises AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTideRises :: AgendaCard TheTideRises
theTideRises = agenda (1, A) TheTideRises Cards.theTideRises (Static 5)

instance RunMessage TheTideRises where
  runMessage msg a@(TheTideRises attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> TheTideRises <$> runMessage msg attrs
