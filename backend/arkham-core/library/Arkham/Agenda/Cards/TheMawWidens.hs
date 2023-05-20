module Arkham.Agenda.Cards.TheMawWidens (
  TheMawWidens (..),
  theMawWidens,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Message
import Arkham.Scenarios.TheEssexCountyExpress.Helpers

newtype TheMawWidens = TheMawWidens AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMawWidens :: AgendaCard TheMawWidens
theMawWidens = agenda (2, A) TheMawWidens Cards.theMawWidens (Static 3)

instance RunMessage TheMawWidens where
  runMessage msg a@(TheMawWidens attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      lead <- getLead
      investigatorIds <- getInvestigatorIds
      lid <- leftmostLocation =<< getJustLocation lead
      pushAll $
        RemoveLocation lid
          : [InvestigatorDiscardAllClues (toSource attrs) iid | iid <- investigatorIds]
            <> [advanceAgendaDeck attrs]
      pure a
    _ -> TheMawWidens <$> runMessage msg attrs
