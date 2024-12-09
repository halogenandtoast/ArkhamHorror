module Arkham.Enemy.Cards.MobGoons (mobGoons, MobGoons (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Strategy

newtype MobGoons = MobGoons EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mobGoons :: EnemyCard MobGoons
mobGoons = enemy MobGoons Cards.mobGoons (3, Static 3, 3) (1, 1)

instance HasModifiersFor MobGoons where
  getModifiersFor (MobGoons a) =
    modifySelf a [AttacksCannotBeCancelled, SetAttackDamageStrategy DamageDirect]

instance RunMessage MobGoons where
  runMessage msg (MobGoons attrs) =
    MobGoons <$> runMessage msg attrs
