module Arkham.Agenda.Cards.AgentsOfTheOuterGods (AgentsOfTheOuterGods (..), agentsOfTheOuterGods) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Prelude

newtype AgentsOfTheOuterGods = AgentsOfTheOuterGods AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agentsOfTheOuterGods :: AgendaCard AgentsOfTheOuterGods
agentsOfTheOuterGods = agenda (2, A) AgentsOfTheOuterGods Cards.agentsOfTheOuterGods (Static 9)

instance RunMessage AgentsOfTheOuterGods where
  runMessage msg a@(AgentsOfTheOuterGods attrs) =
    case msg of
      AdvanceAgenda aid
        | aid == toId attrs && onSide B attrs ->
            a <$ pushAll [advanceAgendaDeck attrs]
      _ -> AgentsOfTheOuterGods <$> runMessage msg attrs
