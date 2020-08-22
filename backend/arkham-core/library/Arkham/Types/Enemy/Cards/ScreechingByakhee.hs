{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.ScreechingByakhee where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import ClassyPrelude

newtype ScreechingByakhee = ScreechingByakhee Attrs
  deriving newtype (Show, ToJSON, FromJSON)

screechingByakhee :: EnemyId -> ScreechingByakhee
screechingByakhee uuid = ScreechingByakhee $ (baseAttrs uuid "01175")
  { enemyHealthDamage = 1
  , enemySanityDamage = 2
  , enemyFight = 3
  , enemyHealth = Static 4
  , enemyEvade = 3
  }

instance (IsInvestigator investigator) => HasActions env investigator ScreechingByakhee where
  getActions i window (ScreechingByakhee attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env ScreechingByakhee where
  runMessage msg (ScreechingByakhee attrs) =
    ScreechingByakhee <$> runMessage msg attrs
