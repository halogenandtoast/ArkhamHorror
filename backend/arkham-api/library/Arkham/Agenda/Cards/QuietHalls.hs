module Arkham.Agenda.Cards.QuietHalls (quietHalls) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Campaign
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype QuietHalls = QuietHalls AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quietHalls :: AgendaCard QuietHalls
quietHalls = agenda (1, A) QuietHalls Cards.quietHalls (Static 7)

instance RunMessage QuietHalls where
  runMessage msg a@(QuietHalls attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        discardCount <- fieldLength InvestigatorDiscard iid
        when (discardCount >= 5) do
          assignHorror iid attrs $ if discardCount >= 10 then 2 else 1
      advanceAgendaDeck attrs

      completedTheHouseAlwaysWins <- any (`elem` ["02062", "51015"]) <$> getCompletedScenarios
      when completedTheHouseAlwaysWins $ push AdvanceCurrentAgenda
      pure a
    _ -> QuietHalls <$> liftRunMessage msg attrs
