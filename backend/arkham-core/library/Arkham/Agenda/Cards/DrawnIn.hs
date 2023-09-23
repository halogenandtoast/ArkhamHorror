module Arkham.Agenda.Cards.DrawnIn (
  DrawnIn (..),
  drawnIn,
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

newtype DrawnIn = DrawnIn AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawnIn :: AgendaCard DrawnIn
drawnIn = agenda (4, A) DrawnIn Cards.drawnIn (Static 3)

instance RunMessage DrawnIn where
  runMessage msg a@(DrawnIn attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      lead <- getLead
      investigatorIds <- getInvestigatorIds
      locationId <- getJustLocation lead
      lid <- leftmostLocation locationId
      rlid <- selectJust (LocationInDirection RightOf $ LocationWithId lid)
      pushAll
        $ RemoveLocation lid
        : RemoveLocation rlid
        : [InvestigatorDiscardAllClues (toSource attrs) iid | iid <- investigatorIds]
          <> [AdvanceAgendaDeck agendaDeckId (toSource attrs)]
      pure a
    _ -> DrawnIn <$> runMessage msg attrs
