module Arkham.Enemy.Cards.CasinoGuardC (casinoGuardC) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype CasinoGuardC = CasinoGuardC EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

casinoGuardC :: EnemyCard CasinoGuardC
casinoGuardC =
  enemy CasinoGuardC Cards.casinoGuardC (3, Static 3, 2) (2, 0)
    & setSpawnAt (NearestLocationToYou $ oneOf ["High Roller's Table", "Guard Room"])

instance HasAbilities CasinoGuardC where
  getAbilities (CasinoGuardC a) =
    extend1 a
      $ restricted a 1 (youExist $ at_ $ locationWithEnemy a)
      $ forced
      $ IncreasedAlarmLevel #after You

instance RunMessage CasinoGuardC where
  runMessage msg e@(CasinoGuardC attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure e
    _ -> CasinoGuardC <$> liftRunMessage msg attrs
