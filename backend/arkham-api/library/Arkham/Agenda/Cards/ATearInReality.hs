module Arkham.Agenda.Cards.ATearInReality (ATearInReality (..), aTearInReality) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Scenarios.TheEssexCountyExpress.Helpers

newtype ATearInReality = ATearInReality AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTearInReality :: AgendaCard ATearInReality
aTearInReality = agenda (1, A) ATearInReality Cards.aTearInReality (Static 4)

instance RunMessage ATearInReality where
  runMessage msg a@(ATearInReality attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      removeLocation =<< leftmostLocation
      eachInvestigator $ discardAllClues attrs
      advanceAgendaDeck attrs
      pure a
    _ -> ATearInReality <$> liftRunMessage msg attrs
