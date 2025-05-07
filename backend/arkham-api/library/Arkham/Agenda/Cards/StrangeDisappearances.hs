module Arkham.Agenda.Cards.StrangeDisappearances (strangeDisappearances) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Choose
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Scenario.Deck

newtype StrangeDisappearances = StrangeDisappearances AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeDisappearances :: AgendaCard StrangeDisappearances
strangeDisappearances = agenda (1, A) StrangeDisappearances Cards.strangeDisappearances (Static 6)

instance RunMessage StrangeDisappearances where
  runMessage msg a@(StrangeDisappearances attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      scenarioDeckCount <- length <$> getScenarioDeck PotentialSacrifices
      when (scenarioDeckCount >= 3) $ randomlyChooseFrom attrs lead PotentialSacrifices 1
      advanceAgendaDeck attrs
      pure a
    ChoseCards _ chose | isTarget attrs chose.target -> do
      placeUnderneath AgendaDeckTarget chose.cards
      pure a
    _ -> StrangeDisappearances <$> liftRunMessage msg attrs
