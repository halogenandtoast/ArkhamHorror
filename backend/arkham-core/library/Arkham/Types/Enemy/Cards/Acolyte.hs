module Arkham.Types.Enemy.Cards.Acolyte
  ( Acolyte(..)
  , acolyte
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Message
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype Acolyte = Acolyte EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acolyte :: EnemyId -> Acolyte
acolyte uuid =
  Acolyte
    $ baseAttrs uuid "01169"
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
