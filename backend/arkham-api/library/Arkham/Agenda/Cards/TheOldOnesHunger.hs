module Arkham.Agenda.Cards.TheOldOnesHunger (theOldOnesHunger) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Choose
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Scenario.Deck

newtype TheOldOnesHunger = TheOldOnesHunger AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOldOnesHunger :: AgendaCard TheOldOnesHunger
theOldOnesHunger = agenda (2, A) TheOldOnesHunger Cards.theOldOnesHunger (Static 6)

instance RunMessage TheOldOnesHunger where
  runMessage msg a@(TheOldOnesHunger attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      scenarioDeckCount <- length <$> getScenarioDeck PotentialSacrifices
      when (scenarioDeckCount >= 2) do
        randomlyChooseFrom attrs lead PotentialSacrifices 1
      advanceAgendaDeck attrs
      pure a
    ChoseCards _ chose | isTarget attrs chose.target -> do
      placeUnderneath AgendaDeckTarget chose.cards
      pure a
    _ -> TheOldOnesHunger <$> liftRunMessage msg attrs
