module Arkham.Homebrew.AgesUnwound.Enemies.Truth (truth) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Trait (Trait (Madness))

newtype Truth = Truth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

truth :: EnemyCard Truth
truth = enemyWith Truth Cards.truth (spawnAtL ?~ SpawnAt (locationIs Locations.thePast))

instance HasAbilities Truth where
  getAbilities (Truth a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage Truth where
  runMessage msg e@(Truth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      searchCollectionForRandomBasicWeakness iid (attrs.ability 1) [Madness]
      placeDoomOnAgenda 1
      pure e
    RequestedPlayerCard iid (isSource attrs -> True) (Just card) _ -> do
      drawCard iid card
      pure e
    _ -> Truth <$> liftRunMessage msg attrs
