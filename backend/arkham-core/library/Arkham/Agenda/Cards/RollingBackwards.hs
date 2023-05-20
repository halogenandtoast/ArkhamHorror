module Arkham.Agenda.Cards.RollingBackwards (
  RollingBackwards (..),
  rollingBackwards,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.TheEssexCountyExpress.Helpers

newtype RollingBackwards = RollingBackwards AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rollingBackwards :: AgendaCard RollingBackwards
rollingBackwards =
  agenda (3, A) RollingBackwards Cards.rollingBackwards (Static 4)

instance RunMessage RollingBackwards where
  runMessage msg a@(RollingBackwards attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      lead <- getLead
      investigatorIds <- getInvestigatorIds
      locationId <- getJustLocation lead
      lid <- leftmostLocation locationId
      rlid <- selectJust $ LocationInDirection RightOf $ LocationWithId lid
      pushAll $
        RemoveLocation lid
          : RemoveLocation rlid
          : [InvestigatorDiscardAllClues (toSource attrs) iid | iid <- investigatorIds]
            <> [AdvanceAgendaDeck agendaDeckId (toSource attrs)]
      pure a
    _ -> RollingBackwards <$> runMessage msg attrs
