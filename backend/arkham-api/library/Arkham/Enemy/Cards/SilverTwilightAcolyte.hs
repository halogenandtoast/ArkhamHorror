module Arkham.Enemy.Cards.SilverTwilightAcolyte (silverTwilightAcolyte) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype SilverTwilightAcolyte = SilverTwilightAcolyte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silverTwilightAcolyte :: EnemyCard SilverTwilightAcolyte
silverTwilightAcolyte =
  enemyWith SilverTwilightAcolyte Cards.silverTwilightAcolyte (2, Static 3, 3) (1, 0)
    $ \a -> a & preyL .~ BearerOf (toId a)

instance HasAbilities SilverTwilightAcolyte where
  getAbilities (SilverTwilightAcolyte a) =
    extend1 a $ forcedAbility a 1 $ EnemyAttacks #after Anyone AnyEnemyAttack (be a)

instance RunMessage SilverTwilightAcolyte where
  runMessage msg e@(SilverTwilightAcolyte attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoomOnAgenda 1
      pure e
    _ -> SilverTwilightAcolyte <$> liftRunMessage msg attrs
