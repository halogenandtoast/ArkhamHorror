{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.Acolyte where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype Acolyte = Acolyte Attrs
  deriving newtype (Show, ToJSON, FromJSON)

acolyte :: EnemyId -> Acolyte
acolyte uuid =
  Acolyte
    $ baseAttrs uuid "01169"
    $ (healthDamage .~ 1)
    . (fight .~ 3)
    . (evade .~ 2)

instance HasModifiersFor env Acolyte where
  getModifiersFor = noModifiersFor

instance HasModifiers env Acolyte where
  getModifiers _ (Acolyte Attrs {..}) = pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env Acolyte where
  getActions i window (Acolyte attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env Acolyte where
  runMessage msg e@(Acolyte attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAtEmptyLocation iid eid
    EnemySpawn _ eid | eid == enemyId ->
      Acolyte <$> runMessage msg (attrs & doom +~ 1)
    _ -> Acolyte <$> runMessage msg attrs
