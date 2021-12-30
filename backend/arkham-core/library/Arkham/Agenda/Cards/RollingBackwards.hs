module Arkham.Agenda.Cards.RollingBackwards
  ( RollingBackwards(..)
  , rollingBackwards
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

newtype RollingBackwards = RollingBackwards AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rollingBackwards :: AgendaCard RollingBackwards
rollingBackwards =
  agenda (3, A) RollingBackwards Cards.rollingBackwards (Static 4)

instance AgendaRunner env => RunMessage env RollingBackwards where
  runMessage msg a@(RollingBackwards attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 3 B -> do
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
    _ -> RollingBackwards <$> runMessage msg attrs
