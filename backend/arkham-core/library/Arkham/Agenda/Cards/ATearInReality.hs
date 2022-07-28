module Arkham.Agenda.Cards.ATearInReality
  ( ATearInReality(..)
  , aTearInReality
  ) where

import Arkham.Prelude

import Arkham.Agenda.Attrs
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Message
import Arkham.Projection
import Arkham.Scenarios.TheEssexCountyExpress.Helpers

newtype ATearInReality = ATearInReality AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTearInReality :: AgendaCard ATearInReality
aTearInReality = agenda (1, A) ATearInReality Cards.aTearInReality (Static 4)

instance RunMessage ATearInReality where
  runMessage msg a@(ATearInReality attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      locationId <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be at location")
        leadInvestigatorId
      lid <- leftmostLocation locationId
      a <$ pushAll
        (RemoveLocation lid
        : [ InvestigatorDiscardAllClues iid | iid <- investigatorIds ]
        <> [AdvanceAgendaDeck agendaDeckId (toSource attrs)]
        )
    _ -> ATearInReality <$> runMessage msg attrs
