module Arkham.Agenda.Cards.CityOfBlood (cityOfBlood) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Placement
import Arkham.Scenarios.TheDepthsOfYoth.Helpers
import Arkham.Zone

newtype CityOfBlood = CityOfBlood AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfBlood :: AgendaCard CityOfBlood
cityOfBlood = agenda (4, A) CityOfBlood Cards.cityOfBlood (Static 4)

instance RunMessage CityOfBlood where
  runMessage msg a@(CityOfBlood attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      fetchCardMaybe [Enemies.harbingerOfValusia, Enemies.harbingerOfValusiaTheSleeperReturns]
        >>= traverse_ \harbinger -> createEnemy_ harbinger (OutOfPlay PursuitZone)
      doStep 1 msg
      advanceAgendaDeck attrs
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      placePursuitEnemies
      pure a
    _ -> CityOfBlood <$> liftRunMessage msg attrs
