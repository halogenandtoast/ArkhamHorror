module Arkham.Agenda.Cards.DrawnIn
  ( DrawnIn(..)
  , drawnIn
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Scenarios.TheEssexCountyExpress.Helpers
import Arkham.Agenda.Attrs
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.LocationId
import Arkham.Message
import Arkham.Query

newtype DrawnIn = DrawnIn AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawnIn :: AgendaCard DrawnIn
drawnIn = agenda (4, A) DrawnIn Cards.drawnIn (Static 3)

instance AgendaRunner env => RunMessage env DrawnIn where
  runMessage msg a@(DrawnIn attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 4 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      investigatorIds <- getInvestigatorIds
      locationId <- getId @LocationId leadInvestigatorId
      lid <- leftmostLocation locationId
      rlid <- fromJustNote "missing right" <$> getId (RightOf, lid)
      a <$ pushAll
        (RemoveLocation lid
        : RemoveLocation rlid
        : [ InvestigatorDiscardAllClues iid | iid <- investigatorIds ]
        <> [AdvanceAgendaDeck agendaDeckId (toSource attrs)]
        )
    _ -> DrawnIn <$> runMessage msg attrs
