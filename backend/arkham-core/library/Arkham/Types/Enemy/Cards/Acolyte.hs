{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.Acolyte where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import ClassyPrelude

newtype Acolyte = Acolyte Attrs
  deriving newtype (Show, ToJSON, FromJSON)

acolyte :: EnemyId -> Acolyte
acolyte uuid = Acolyte $ (baseAttrs uuid "01169")
  { enemyHealthDamage = 1
  , enemyFight = 3
  , enemyEvade = 2
  }

instance (EnemyRunner env) => RunMessage env Acolyte where
  runMessage msg (Acolyte attrs) = Acolyte <$> runMessage msg attrs
