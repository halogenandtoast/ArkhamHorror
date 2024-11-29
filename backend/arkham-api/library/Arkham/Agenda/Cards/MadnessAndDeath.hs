module Arkham.Agenda.Cards.MadnessAndDeath (MadnessAndDeath (..), madnessAndDeath) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype MadnessAndDeath = MadnessAndDeath AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madnessAndDeath :: AgendaCard MadnessAndDeath
madnessAndDeath = agenda (6, A) MadnessAndDeath Cards.madnessAndDeath (Static 12)

instance RunMessage MadnessAndDeath where
  runMessage msg a@(MadnessAndDeath attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      push R1
      pure a
    _ -> MadnessAndDeath <$> liftRunMessage msg attrs
