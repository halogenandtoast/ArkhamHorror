module Arkham.Event.Events.Evidence1 (evidence1) where

import Arkham.Enemy.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Calculation
import Arkham.History

newtype Evidence1 = Evidence1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evidence1 :: EventCard Evidence1
evidence1 = event Evidence1 Cards.evidence1

instance RunMessage Evidence1 where
  runMessage msg e@(Evidence1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      defeated <- getHistoryField #turn iid HistoryEnemiesDefeated
      totalPrintedHealth <- sumM (calculatePrinted . attr enemyHealth) defeated
      discoverAtYourLocation NotInvestigate iid attrs $ if totalPrintedHealth >= 4 then 2 else 1
      pure e
    _ -> Evidence1 <$> liftRunMessage msg attrs
