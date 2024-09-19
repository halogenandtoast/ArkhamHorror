module Arkham.Enemy.Cards.CatacombsDocent (catacombsDocent, CatacombsDocent (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CatacombsDocent = CatacombsDocent EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catacombsDocent :: EnemyCard CatacombsDocent
catacombsDocent =
  enemyWith CatacombsDocent Cards.catacombsDocent (3, Static 2, 2) (0, 1)
    $ spawnAtL
    ?~ SpawnAt (NearestLocationToYou UnrevealedLocation)

instance HasAbilities CatacombsDocent where
  getAbilities (CatacombsDocent a) =
    extend
      a
      [skillTestAbility $ restricted a 1 (OnSameLocation <> exists UnrevealedLocation) parleyAction_]

instance RunMessage CatacombsDocent where
  runMessage msg e@(CatacombsDocent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 4)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      unrevealedLocations <- select UnrevealedLocation
      chooseTargetM iid unrevealedLocations $ push . LookAtRevealed iid (toSource attrs) . toTarget
      pure e
    _ -> CatacombsDocent <$> liftRunMessage msg attrs
