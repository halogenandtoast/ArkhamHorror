module Arkham.Enemy.Cards.HarbingerOfValusia
  ( harbingerOfValusia
  , HarbingerOfValusia(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype HarbingerOfValusia = HarbingerOfValusia EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

harbingerOfValusia :: EnemyCard HarbingerOfValusia
harbingerOfValusia = enemy HarbingerOfValusia Cards.harbingerOfValusia (3, PerPlayer 10, 3) (2, 2)

instance RunMessage HarbingerOfValusia where
  runMessage msg (HarbingerOfValusia attrs) =
    HarbingerOfValusia <$> runMessage msg attrs
