{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.RelentlessDarkYoung where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import ClassyPrelude

newtype RelentlessDarkYoung = RelentlessDarkYoung Attrs
  deriving newtype (Show, ToJSON, FromJSON)

relentlessDarkYoung :: EnemyId -> RelentlessDarkYoung
relentlessDarkYoung uuid = RelentlessDarkYoung $ (baseAttrs uuid "01179")
  { enemyHealthDamage = 2
  , enemySanityDamage = 1
  , enemyFight = 4
  , enemyHealth = Static 5
  , enemyEvade = 2
  }

instance (IsInvestigator investigator) => HasActions env investigator RelentlessDarkYoung where
  getActions i window (RelentlessDarkYoung attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env RelentlessDarkYoung where
  runMessage msg (RelentlessDarkYoung attrs) =
    RelentlessDarkYoung <$> runMessage msg attrs
