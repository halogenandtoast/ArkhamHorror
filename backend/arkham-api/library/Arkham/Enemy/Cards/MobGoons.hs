module Arkham.Enemy.Cards.MobGoons (mobGoons) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Strategy

newtype MobGoons = MobGoons EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mobGoons :: EnemyCard MobGoons
mobGoons = enemyWith MobGoons Cards.mobGoons (3, Static 3, 3) (1, 1) preyIsOnlyBearer

instance HasModifiersFor MobGoons where
  getModifiersFor (MobGoons a) =
    modifySelf a [AttacksCannotBeCancelled, SetAttackDamageStrategy DamageDirect]
