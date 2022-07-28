module Arkham.Agenda.Cards.DrawnIn
  ( DrawnIn(..)
  , drawnIn
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

newtype DrawnIn = DrawnIn AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawnIn :: AgendaCard DrawnIn
drawnIn = agenda (4, A) DrawnIn Cards.drawnIn (Static 3)

instance RunMessage DrawnIn where
  runMessage msg a@(DrawnIn attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      locationId <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be at a location")
        leadInvestigatorId
      lid <- leftmostLocation locationId
      rlid <- selectJust (LocationInDirection RightOf $ LocationWithId lid)
      a <$ pushAll
        (RemoveLocation lid
        : RemoveLocation rlid
        : [ InvestigatorDiscardAllClues iid | iid <- investigatorIds ]
        <> [AdvanceAgendaDeck agendaDeckId (toSource attrs)]
        )
    _ -> DrawnIn <$> runMessage msg attrs
