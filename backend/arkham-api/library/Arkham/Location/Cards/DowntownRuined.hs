module Arkham.Location.Cards.DowntownRuined (downtownRuined) where

import Arkham.Ability
import Arkham.Helpers.Window (getEnemy)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (StarSpawn))

newtype DowntownRuined = DowntownRuined LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownRuined :: LocationCard DowntownRuined
downtownRuined = location DowntownRuined Cards.downtownRuined 4 (Static 1)

instance HasAbilities DowntownRuined where
  getAbilities (DowntownRuined a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ triggered (EnemyEnters #when (be a) (EnemyWithTrait StarSpawn)) (ResourceCost 3)

instance RunMessage DowntownRuined where
  runMessage msg l@(DowntownRuined attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getEnemy -> enemy) _ -> do
      automaticallyEvadeEnemy iid enemy
      pure l
    _ -> DowntownRuined <$> liftRunMessage msg attrs
