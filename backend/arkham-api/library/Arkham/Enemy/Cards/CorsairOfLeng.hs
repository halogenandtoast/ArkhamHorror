module Arkham.Enemy.Cards.CorsairOfLeng (corsairOfLeng, CorsairOfLeng (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers qualified as Mods
import Arkham.Matcher
import Arkham.Trait (Trait (City, Surface))

newtype CorsairOfLeng = CorsairOfLeng EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corsairOfLeng :: EnemyCard CorsairOfLeng
corsairOfLeng =
  enemyWith CorsairOfLeng Cards.corsairOfLeng (2, Static 4, 5) (1, 0)
    $ spawnAtL
    ?~ SpawnAt (NearestLocationToYou $ oneOf [withTrait City, withTrait Surface])

instance HasAbilities CorsairOfLeng where
  getAbilities (CorsairOfLeng a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after Anyone AttackViaAlert (be a)

instance RunMessage CorsairOfLeng where
  runMessage msg e@(CorsairOfLeng attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      phaseModifier (attrs.ability 1) attrs (Mods.EnemyEvade (-3))
      pure e
    _ -> CorsairOfLeng <$> liftRunMessage msg attrs
