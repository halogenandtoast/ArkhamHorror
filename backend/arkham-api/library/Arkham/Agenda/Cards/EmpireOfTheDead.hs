module Arkham.Agenda.Cards.EmpireOfTheDead (empireOfTheDead) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query

newtype EmpireOfTheDead = EmpireOfTheDead AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

empireOfTheDead :: AgendaCard EmpireOfTheDead
empireOfTheDead = agenda (1, A) EmpireOfTheDead Cards.empireOfTheDead (Static 6)

instance RunMessage EmpireOfTheDead where
  runMessage msg a@(EmpireOfTheDead attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      drawCard lead =<< genCard Enemies.specterOfDeath
      advanceAgendaDeck attrs
      pure a
    _ -> EmpireOfTheDead <$> liftRunMessage msg attrs
