module Arkham.Event.Events.CunningDistraction where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher hiding (EnemyEvaded)

newtype CunningDistraction = CunningDistraction EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cunningDistraction :: EventCard CunningDistraction
cunningDistraction = event CunningDistraction Cards.cunningDistraction

instance RunMessage CunningDistraction where
  runMessage msg e@(CunningDistraction attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectEach (enemyAtLocationWith iid) (automaticallyEvadeEnemy iid)
      pure e
    _ -> CunningDistraction <$> liftRunMessage msg attrs
