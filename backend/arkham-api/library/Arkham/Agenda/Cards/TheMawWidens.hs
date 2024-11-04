module Arkham.Agenda.Cards.TheMawWidens (TheMawWidens (..), theMawWidens) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Scenarios.TheEssexCountyExpress.Helpers

newtype TheMawWidens = TheMawWidens AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMawWidens :: AgendaCard TheMawWidens
theMawWidens = agenda (2, A) TheMawWidens Cards.theMawWidens (Static 3)

instance RunMessage TheMawWidens where
  runMessage msg a@(TheMawWidens attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      removeLocation =<< leftmostLocation
      eachInvestigator $ discardAllClues attrs
      advanceAgendaDeck attrs
      pure a
    _ -> TheMawWidens <$> liftRunMessage msg attrs
