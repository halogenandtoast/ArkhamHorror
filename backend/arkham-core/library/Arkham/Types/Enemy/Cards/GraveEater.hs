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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveEater :: EnemyCard GraveEater
graveEater = enemy GraveEater Cards.graveEater (2, Static 2, 2) (1, 1)

instance HasModifiersFor env GraveEater where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env GraveEater where
  getActions i window (GraveEater attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GraveEater where
  runMessage msg e@(GraveEater attrs) = case msg of
    After (EnemyAttack iid eid) | eid == enemyId attrs ->
      e <$ unshiftMessage (RandomDiscard iid)
    _ -> GraveEater <$> runMessage msg attrs
