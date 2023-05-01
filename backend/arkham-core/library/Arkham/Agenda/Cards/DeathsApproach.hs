module Arkham.Agenda.Cards.DeathsApproach
  ( DeathsApproach(..)
  , deathsApproach
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype DeathsApproach = DeathsApproach AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deathsApproach :: AgendaCard DeathsApproach
deathsApproach = agenda (2, A) DeathsApproach Cards.deathsApproach (Static 7)

instance RunMessage DeathsApproach where
  runMessage msg a@(DeathsApproach attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> DeathsApproach <$> runMessage msg attrs
