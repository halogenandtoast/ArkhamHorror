{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.Acolyte where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.Message
import ClassyPrelude
import Lens.Micro

newtype Acolyte = Acolyte Attrs
  deriving newtype (Show, ToJSON, FromJSON)

acolyte :: EnemyId -> Acolyte
acolyte uuid = Acolyte $ (baseAttrs uuid "01169")
  { enemyHealthDamage = 1
  , enemyFight = 3
  , enemyEvade = 2
  }

instance (IsInvestigator investigator) => HasActions env investigator Acolyte where
  getActions i (Acolyte attrs) = getActions i attrs

instance (EnemyRunner env) => RunMessage env Acolyte where
  runMessage msg e@(Acolyte attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAtEmptyLocation iid eid
    EnemySpawn _ eid | eid == enemyId ->
      Acolyte <$> runMessage msg (attrs & doom +~ 1)
    _ -> Acolyte <$> runMessage msg attrs
