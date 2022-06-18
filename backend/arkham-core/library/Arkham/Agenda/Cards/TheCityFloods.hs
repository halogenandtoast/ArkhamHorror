module Arkham.Agenda.Cards.TheCityFloods
  ( TheCityFloods
  , theCityFloods
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheCityFloods = TheCityFloods AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCityFloods :: AgendaCard TheCityFloods
theCityFloods = agenda (3, A) TheCityFloods Cards.theCityFloods (Static 8)

instance RunMessage TheCityFloods where
  runMessage msg a@(TheCityFloods attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> TheCityFloods <$> runMessage msg attrs
