module Arkham.Agenda.Cards.CityOfBlood (cityOfBlood) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log (whenHasRecord)
import Arkham.Helpers.Scenario (getIsReturnTo)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
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
      whenHasRecord TheHarbingerIsStillAlive do
        isReturnTo <- getIsReturnTo
        let version = if isReturnTo then Enemies.harbingerOfValusiaTheSleeperReturns else Enemies.harbingerOfValusia
        selectOne (OutOfPlayEnemy SetAsideZone $ enemyIs version) >>= \case
          Just enemy -> place enemy (OutOfPlay PursuitZone)
          Nothing -> whenJustM (fetchCardMaybe version) (`createEnemy_` OutOfPlay PursuitZone)
      doStep 1 msg
      advanceAgendaDeck attrs
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      placePursuitEnemies
      pure a
    _ -> CityOfBlood <$> liftRunMessage msg attrs
