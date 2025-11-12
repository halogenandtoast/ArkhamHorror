module Arkham.Enemy.Cards.CasinoGuardB (casinoGuardB) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype CasinoGuardB = CasinoGuardB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

casinoGuardB :: EnemyCard CasinoGuardB
casinoGuardB =
  enemy CasinoGuardB Cards.casinoGuardB (3, Static 3, 2) (2, 0)
    & setSpawnAt (NearestLocationToYou $ oneOf ["High Roller's Table", "Guard Room"])

instance HasAbilities CasinoGuardB where
  getAbilities (CasinoGuardB a) =
    extend1 a
      $ restricted a 1 (youExist $ at_ $ locationWithEnemy a)
      $ forced
      $ IncreasedAlarmLevel #after You

instance RunMessage CasinoGuardB where
  runMessage msg e@(CasinoGuardB attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure e
    _ -> CasinoGuardB <$> liftRunMessage msg attrs
