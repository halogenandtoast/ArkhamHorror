{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.HuntingNightgaunt where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import ClassyPrelude

newtype HuntingNightgaunt = HuntingNightgaunt Attrs
  deriving newtype (Show, ToJSON, FromJSON)

huntingNightgaunt :: EnemyId -> HuntingNightgaunt
huntingNightgaunt uuid = HuntingNightgaunt $ (baseAttrs uuid "01172")
  { enemyHealthDamage = 1
  , enemySanityDamage = 1
  , enemyFight = 3
  , enemyHealth = Static 4
  , enemyEvade = 1
  }

instance (EnemyRunner env) => RunMessage env HuntingNightgaunt where
  runMessage msg (HuntingNightgaunt attrs) =
    HuntingNightgaunt <$> runMessage msg attrs
