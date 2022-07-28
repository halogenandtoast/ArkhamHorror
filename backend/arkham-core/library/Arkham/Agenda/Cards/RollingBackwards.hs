module Arkham.Agenda.Cards.RollingBackwards
  ( RollingBackwards(..)
  , rollingBackwards
  ) where

import Arkham.Prelude

import Arkham.Agenda.Attrs
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
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
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      locationId <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be at a location")
        leadInvestigatorId
      lid <- leftmostLocation locationId
      rlid <- selectJust $ LocationInDirection RightOf $ LocationWithId lid
      a <$ pushAll
        (RemoveLocation lid
        : RemoveLocation rlid
        : [ InvestigatorDiscardAllClues iid | iid <- investigatorIds ]
        <> [AdvanceAgendaDeck agendaDeckId (toSource attrs)]
        )
    _ -> RollingBackwards <$> runMessage msg attrs
