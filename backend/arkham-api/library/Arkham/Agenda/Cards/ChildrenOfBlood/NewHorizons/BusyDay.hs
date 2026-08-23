module Arkham.Agenda.Cards.ChildrenOfBlood.NewHorizons.BusyDay (busyDay) where

import Arkham.Agenda.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Enemies
import Arkham.Helpers.Enemy
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype BusyDay = BusyDay AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

busyDay :: AgendaCard BusyDay
busyDay = agenda (1, A) BusyDay Cards.busyDay (Static 5)

instance RunMessage BusyDay where
  runMessage msg a@(BusyDay attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      javier <- getSetAsideCard Enemies.javierRivera
      investigators <- select MostClues
      leadChooseOrRunOneM $ targets investigators \iid ->
        createEnemyWith_ javier Unplaced (createEngagedWith iid)
      advanceAgendaDeck attrs
      pure a
    _ -> BusyDay <$> liftRunMessage msg attrs
