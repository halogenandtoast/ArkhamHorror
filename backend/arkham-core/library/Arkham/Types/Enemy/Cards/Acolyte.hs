module Arkham.Types.Enemy.Cards.Acolyte
  ( Acolyte(..)
  , acolyte
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message

newtype Acolyte = Acolyte EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acolyte :: EnemyCard Acolyte
acolyte = enemy Acolyte Cards.acolyte
  $ (healthDamageL .~ 1)
  . (fightL .~ 3)
  . (evadeL .~ 2)

instance HasModifiersFor env Acolyte where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Acolyte where
  getActions i window (Acolyte attrs) = getActions i window attrs

instance EnemyRunner env => RunMessage env Acolyte where
  runMessage msg e@(Acolyte attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAtEmptyLocation iid eid
    EnemySpawn _ _ eid | eid == enemyId ->
      Acolyte <$> runMessage msg (attrs & doomL +~ 1)
    _ -> Acolyte <$> runMessage msg attrs
