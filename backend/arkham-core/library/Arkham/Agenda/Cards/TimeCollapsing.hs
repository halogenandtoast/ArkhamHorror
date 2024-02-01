module Arkham.Agenda.Cards.TimeCollapsing (
  TimeCollapsing (..),
  timeCollapsing,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Resolution

newtype TimeCollapsing = TimeCollapsing AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

timeCollapsing :: AgendaCard TimeCollapsing
timeCollapsing = agenda (3, A) TimeCollapsing Cards.timeCollapsing (Static 6)

instance RunMessage TimeCollapsing where
  runMessage msg a@(TimeCollapsing attrs) = case msg of
    AdvanceAgenda aid
      | aid == toId attrs && onSide B attrs ->
          a <$ pushAll [ScenarioResolution $ Resolution 2]
    _ -> TimeCollapsing <$> runMessage msg attrs
