module Arkham.Treachery.Cards.WatchersGrasp (watchersGrasp) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WatchersGrasp = WatchersGrasp TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watchersGrasp :: TreacheryCard WatchersGrasp
watchersGrasp = treachery WatchersGrasp Cards.watchersGrasp

instance HasModifiersFor WatchersGrasp where
  getModifiersFor (WatchersGrasp a) = do
    modifySelect a (enemyIs Enemies.theSpectralWatcher) [ForcePrey $ Prey $ be a.drawnBy]

instance RunMessage WatchersGrasp where
  runMessage msg t@(WatchersGrasp attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      theSpectralWatcher <- selectJust (enemyIs Enemies.theSpectralWatcher)

      healDamage theSpectralWatcher attrs 3
      readyThis theSpectralWatcher
      sendMessage theSpectralWatcher HuntersMove
      isReturnTo <- getIsReturnTo
      if isReturnTo
        then temporaryModifier theSpectralWatcher ScenarioSource DoNotExhaust do
          sendMessage theSpectralWatcher EnemiesAttack
        else sendMessage theSpectralWatcher EnemiesAttack
      pure t
    _ -> WatchersGrasp <$> liftRunMessage msg attrs
