module Arkham.Enemy.Cards.CasinoGuardA (casinoGuardA) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype CasinoGuardA = CasinoGuardA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

casinoGuardA :: EnemyCard CasinoGuardA
casinoGuardA =
  enemy CasinoGuardA Cards.casinoGuardA (3, Static 3, 2) (2, 0)
    & setSpawnAt (NearestLocationToYou $ oneOf ["High Roller's Table", "Guard Room"])

instance HasAbilities CasinoGuardA where
  getAbilities (CasinoGuardA a) =
    extend1 a
      $ restricted a 1 (youExist $ at_ $ locationWithEnemy a)
      $ forced
      $ IncreasedAlarmLevel #after You

instance RunMessage CasinoGuardA where
  runMessage msg e@(CasinoGuardA attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure e
    _ -> CasinoGuardA <$> liftRunMessage msg attrs
