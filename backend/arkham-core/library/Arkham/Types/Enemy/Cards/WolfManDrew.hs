{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.WolfManDrew where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Message
import ClassyPrelude
import Lens.Micro

newtype WolfManDrew = WolfManDrew Attrs
  deriving newtype (Show, ToJSON, FromJSON)

wolfManDrew :: EnemyId -> WolfManDrew
wolfManDrew uuid = WolfManDrew $ (baseAttrs uuid "01137")
  { enemyHealthDamage = 2
  , enemyFight = 4
  , enemyHealth = Static 4
  , enemyEvade = 2
  , enemyVictory = Just 1
  }

instance (IsInvestigator investigator) => HasActions investigator WolfManDrew where
  getActions i (WolfManDrew attrs) = getActions i attrs

instance (EnemyRunner env) => RunMessage env WolfManDrew where
  runMessage msg e@(WolfManDrew attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAtOneOf iid eid ["01130", "01131"]
    PerformEnemyAttack _ eid | eid == enemyId ->
      WolfManDrew <$> runMessage msg (attrs & damage %~ max 0 . subtract 1)
    _ -> WolfManDrew <$> runMessage msg attrs
