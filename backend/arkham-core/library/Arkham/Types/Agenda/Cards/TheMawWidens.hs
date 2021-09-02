module Arkham.Types.Agenda.Cards.TheMawWidens
  ( TheMawWidens(..)
  , theMawWidens
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Scenarios.TheEssexCountyExpress.Helpers
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query

newtype TheMawWidens = TheMawWidens AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMawWidens :: AgendaCard TheMawWidens
theMawWidens = agenda (2, A) TheMawWidens Cards.theMawWidens (Static 3)

instance AgendaRunner env => RunMessage env TheMawWidens where
  runMessage msg a@(TheMawWidens attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      investigatorIds <- getInvestigatorIds
      locationId <- getId @LocationId leadInvestigatorId
      lid <- leftmostLocation locationId
      a <$ pushAll
        (RemoveLocation lid
        : [ InvestigatorDiscardAllClues iid | iid <- investigatorIds ]
        <> [NextAgenda agendaId "02162"]
        )
    _ -> TheMawWidens <$> runMessage msg attrs
