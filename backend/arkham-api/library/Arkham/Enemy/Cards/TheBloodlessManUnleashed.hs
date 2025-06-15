module Arkham.Enemy.Cards.TheBloodlessManUnleashed (
  theBloodlessManUnleashed,
  TheBloodlessManUnleashed(..),
) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Keyword
import Arkham.Trait

newtype TheBloodlessManUnleashed = TheBloodlessManUnleashed EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBloodlessManUnleashed :: EnemyCard TheBloodlessManUnleashed

theBloodlessManUnleashed =
  enemyWith
    TheBloodlessManUnleashed
    Cards.theBloodlessManUnleashed
    (3, PerPlayer 6, 3)
    (2, 1)
    \attrs -> attrs & (keywordsL %~ setFromList . (Hunter :) . (Massive :))

instance HasModifiersFor TheBloodlessManUnleashed where
  getModifiersFor _ = pure []

instance HasAbilities TheBloodlessManUnleashed where
  getAbilities (TheBloodlessManUnleashed a) = withBaseAbilities a [
      mkAbility a 1
        $ forced
        $ EnemyAttacks #after You AnyEnemyAttack (be a)
    ]

instance RunMessage TheBloodlessManUnleashed where
  runMessage msg e@(TheBloodlessManUnleashed attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      removeCluesFromPlay <- selectOne $ assetControlledBy iid <> AssetWithTrait Guest
      for_ removeCluesFromPlay \aid -> removeFromGame aid
      pure e
    _ -> TheBloodlessManUnleashed <$> liftRunMessage msg attrs
