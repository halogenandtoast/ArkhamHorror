module Arkham.Types.Enemy.Cards.GraveEater
  ( graveEater
  , GraveEater(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message

newtype GraveEater = GraveEater EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities env)

graveEater :: EnemyCard GraveEater
graveEater = enemy GraveEater Cards.graveEater (2, Static 2, 2) (1, 1)

instance EnemyRunner env => RunMessage env GraveEater where
  runMessage msg e@(GraveEater attrs) = case msg of
    After (EnemyAttack iid eid _) | eid == enemyId attrs ->
      e <$ push (RandomDiscard iid)
    _ -> GraveEater <$> runMessage msg attrs
