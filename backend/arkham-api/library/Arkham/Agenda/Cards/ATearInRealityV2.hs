module Arkham.Agenda.Cards.ATearInRealityV2 (aTearInRealityV2) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Scenarios.TheEssexCountyExpress.Helpers

newtype ATearInRealityV2 = ATearInRealityV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTearInRealityV2 :: AgendaCard ATearInRealityV2
aTearInRealityV2 = agenda (1, A) ATearInRealityV2 Cards.aTearInRealityV2 (Static 2)

instance RunMessage ATearInRealityV2 where
  runMessage msg a@(ATearInRealityV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      removeLocation =<< leftmostLocation
      eachInvestigator $ discardAllClues attrs
      advanceAgendaDeck attrs
      pure a
    _ -> ATearInRealityV2 <$> liftRunMessage msg attrs
