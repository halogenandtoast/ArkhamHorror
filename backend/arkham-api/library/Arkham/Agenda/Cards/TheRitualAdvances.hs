module Arkham.Agenda.Cards.TheRitualAdvances (TheRitualAdvances (..), theRitualAdvances) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers

newtype TheRitualAdvances = TheRitualAdvances AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRitualAdvances :: AgendaCard TheRitualAdvances
theRitualAdvances = agenda (3, A) TheRitualAdvances Cards.theRitualAdvances (Static 6)

instance HasAbilities TheRitualAdvances where
  getAbilities (TheRitualAdvances a) = [needsAir a 1]

instance RunMessage TheRitualAdvances where
  runMessage msg a@(TheRitualAdvances attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      record DagonHasAwakened
      push R1
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    _ -> TheRitualAdvances <$> liftRunMessage msg attrs
