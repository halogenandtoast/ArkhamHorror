module Arkham.Event.Events.BuryThemDeep (buryThemDeep) where

import Arkham.Enemy.Import.Lifted (insteadOfDefeatWithWindows)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (defeatedEnemy)

newtype BuryThemDeep = BuryThemDeep EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

buryThemDeep :: EventCard BuryThemDeep
buryThemDeep = event BuryThemDeep Cards.buryThemDeep

instance RunMessage BuryThemDeep where
  runMessage msg e@(BuryThemDeep attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      addToVictory iid attrs
      let enemyId = defeatedEnemy attrs.windows
      insteadOfDefeatWithWindows enemyId $ addToVictory iid enemyId
      pure e
    _ -> BuryThemDeep <$> liftRunMessage msg attrs
