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

{- | /Truth (Words Will Never Hurt You?)/, the reverse of the
@:ages-unwound:194@ printing of /Secrets Long Forgotten/.

"__Spawn__ - The Past. __Alert__. __Hunter__." The keywords are on the card def.
-}
truth :: EnemyCard Truth
truth = enemyWith Truth Cards.truth (spawnAtL ?~ SpawnAt (locationIs Locations.thePast))

{- | "__Forced__ - After Secrets Long Forgotten attacks you: Search the collection
for a random basic [[Madness]] weakness and draw it. Place 1 doom on the current
agenda."

(The card names itself by the location it is printed on the back of.)
-}
instance HasAbilities Truth where
  getAbilities (Truth a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage Truth where
  runMessage msg e@(Truth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      searchCollectionForRandomBasicWeakness iid (attrs.ability 1) [Madness]
      placeDoomOnAgenda 1
      pure e
    _ -> Truth <$> liftRunMessage msg attrs
