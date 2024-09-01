module Arkham.Enemy.Cards.Hoods (hoods, Hoods (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Matcher

newtype Hoods = Hoods EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hoods :: EnemyCard Hoods
hoods =
  enemyWith Hoods Cards.hoods (3, Static 3, 3) (1, 1)
    $ \a -> a & preyL .~ BearerOf (toId a)

instance HasAbilities Hoods where
  getAbilities (Hoods a) =
    withBaseAbilities a [mkAbility a 1 $ forced $ EnemyEvaded #after You (be a)]

instance RunMessage Hoods where
  runMessage msg e@(Hoods attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> Hoods <$> liftRunMessage msg attrs
