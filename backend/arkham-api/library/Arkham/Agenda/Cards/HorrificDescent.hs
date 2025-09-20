module Arkham.Agenda.Cards.HorrificDescent (horrificDescent) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheDepthsOfYoth.Helpers

newtype HorrificDescent = HorrificDescent AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

horrificDescent :: AgendaCard HorrificDescent
horrificDescent = agenda (2, A) HorrificDescent Cards.horrificDescent (Static 3)

instance RunMessage HorrificDescent where
  runMessage msg a@(HorrificDescent attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      placePursuitEnemies
      whenM (getAnyHasSupply Canteen) $ remember CollectedAStrangeLiquid
      advanceAgendaDeck attrs
      pure a
    _ -> HorrificDescent <$> liftRunMessage msg attrs
