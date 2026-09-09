module Arkham.Homebrew.DarkMatter.Enemies.TheBOOGEYMAN (theBOOGEYMAN) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards

newtype TheBOOGEYMAN = TheBOOGEYMAN EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theBOOGEYMAN :: EnemyCard TheBOOGEYMAN
theBOOGEYMAN = enemy TheBOOGEYMAN Cards.theBOOGEYMAN

instance HasModifiersFor TheBOOGEYMAN where
  getModifiersFor (TheBOOGEYMAN a) =
    modifySelf a [CannotBeAttacked, CannotBeDamaged, CannotBeEvaded]

instance RunMessage TheBOOGEYMAN where
  runMessage msg (TheBOOGEYMAN attrs) =
    runQueueT $ TheBOOGEYMAN <$> liftRunMessage msg attrs
