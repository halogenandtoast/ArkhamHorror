module Arkham.Agenda.Cards.TheDescentBegins
  ( TheDescentBegins(..)
  , theDescentBegins
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Message
import Arkham.Placement
import Arkham.Scenarios.TheDepthsOfYoth.Helpers

newtype TheDescentBegins = TheDescentBegins AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDescentBegins :: AgendaCard TheDescentBegins
theDescentBegins =
  agenda (1, A) TheDescentBegins Cards.theDescentBegins (Static 3)

instance RunMessage TheDescentBegins where
  runMessage msg a@(TheDescentBegins attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      choices <- toList <$> getInPursuitEnemyWithHighestEvade
      lead <- getLeadInvestigatorId
      depthStart <- getDepthStart
      pushAll $ if null choices
        then [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
        else
          [ chooseOrRunOne
            lead
            [ targetLabel choice [PlaceEnemy choice $ AtLocation depthStart]
            | choice <- choices
            ]
          , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
          ]
      pure a
    _ -> TheDescentBegins <$> runMessage msg attrs
