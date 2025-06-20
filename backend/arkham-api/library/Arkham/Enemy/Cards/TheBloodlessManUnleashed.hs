module Arkham.Enemy.Cards.TheBloodlessManUnleashed (theBloodlessManUnleashed) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Trait

newtype TheBloodlessManUnleashed = TheBloodlessManUnleashed EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBloodlessManUnleashed :: EnemyCard TheBloodlessManUnleashed
theBloodlessManUnleashed =
  enemy
    TheBloodlessManUnleashed
    Cards.theBloodlessManUnleashed
    (3, PerPlayer 6, 3)
    (2, 1)

instance HasAbilities TheBloodlessManUnleashed where
  getAbilities (TheBloodlessManUnleashed a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage TheBloodlessManUnleashed where
  runMessage msg e@(TheBloodlessManUnleashed attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mGuest <- selectOne $ assetControlledBy iid <> AssetWithTrait Guest
      for_ mGuest removeFromGame
      pure e
    _ -> TheBloodlessManUnleashed <$> liftRunMessage msg attrs
