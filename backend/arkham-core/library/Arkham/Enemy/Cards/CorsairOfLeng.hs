module Arkham.Enemy.Cards.CorsairOfLeng (
  corsairOfLeng,
  CorsairOfLeng (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.Modifiers qualified as Mods
import Arkham.Matcher
import Arkham.Trait (Trait (City, Surface))

newtype CorsairOfLeng = CorsairOfLeng EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

corsairOfLeng :: EnemyCard CorsairOfLeng
corsairOfLeng =
  enemyWith CorsairOfLeng Cards.corsairOfLeng (2, Static 4, 5) (1, 0)
    $ spawnAtL
    ?~ SpawnAt (NearestLocationToYou $ oneOf [withTrait City, withTrait Surface])

instance HasAbilities CorsairOfLeng where
  getAbilities (CorsairOfLeng attrs) =
    extend
      attrs
      [mkAbility attrs 1 $ ForcedAbility $ EnemyAttacks #after Anyone AttackViaAlert $ be attrs]

instance RunMessage CorsairOfLeng where
  runMessage msg e@(CorsairOfLeng attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ phaseModifier (attrs.ability 1) attrs (Mods.EnemyEvade (-3))
      pure e
    _ -> CorsairOfLeng <$> runMessage msg attrs
