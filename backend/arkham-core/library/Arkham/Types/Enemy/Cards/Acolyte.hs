module Arkham.Types.Enemy.Cards.Acolyte
  ( Acolyte(..)
  , acolyte
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message

newtype Acolyte = Acolyte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions)

acolyte :: EnemyCard Acolyte
acolyte = enemy Acolyte Cards.acolyte (3, Static 1, 2) (1, 0)

instance EnemyRunner env => RunMessage env Acolyte where
  runMessage msg e@(Acolyte attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAtEmptyLocation iid eid
    EnemySpawn _ _ eid | eid == enemyId ->
      Acolyte <$> runMessage msg (attrs & doomL +~ 1)
    _ -> Acolyte <$> runMessage msg attrs
