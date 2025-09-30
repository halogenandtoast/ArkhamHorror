module Arkham.Treachery.Cards.WatchersGazeUnionAndDisillusion (watchersGazeUnionAndDisillusion) where

import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.UnionAndDisillusion.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WatchersGazeUnionAndDisillusion = WatchersGazeUnionAndDisillusion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watchersGazeUnionAndDisillusion :: TreacheryCard WatchersGazeUnionAndDisillusion
watchersGazeUnionAndDisillusion = treachery WatchersGazeUnionAndDisillusion Cards.watchersGazeUnionAndDisillusion

instance RunMessage WatchersGazeUnionAndDisillusion where
  runMessage msg t@(WatchersGazeUnionAndDisillusion attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      eachInvestigator \iid -> do
        sid <- getRandom
        revelationSkillTest sid iid attrs #willpower (Fixed 5)
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      yourLocationIsHaunted <- selectAny $ locationWithInvestigator iid <> HauntedLocation
      watchersLocationIsHaunted <-
        selectAny $ LocationWithEnemy (enemyIs Enemies.theSpectralWatcher) <> HauntedLocation
      chooseOrRunOneM iid $ scenarioI18n do
        when yourLocationIsHaunted do
          labeled' "watchersGaze.yourLocation" $ handleTarget iid attrs iid
        when watchersLocationIsHaunted do
          labeled' "watchersGaze.watchersLocation" $ handleTarget iid attrs attrs
      pure t
    HandleTargetChoice iid (isSource attrs -> True) (InvestigatorTarget _) -> do
      runHauntedAbilities iid
      pure t
    HandleTargetChoice iid (isSource attrs -> True) (TreacheryTarget _) -> do
      lid <- selectJust $ LocationWithEnemy (enemyIs Enemies.theSpectralWatcher)
      runLocationHauntedAbilities iid lid
      pure t
    _ -> WatchersGazeUnionAndDisillusion <$> liftRunMessage msg attrs
