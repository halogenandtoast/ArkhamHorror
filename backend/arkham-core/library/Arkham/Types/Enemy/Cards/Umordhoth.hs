{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.Umordhoth where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import ClassyPrelude

newtype Umordhoth = Umordhoth Attrs
  deriving newtype (Show, ToJSON, FromJSON)

umordhoth :: EnemyId -> Umordhoth
umordhoth uuid = Umordhoth $ (baseAttrs uuid "01157")
  { enemyHealthDamage = 3
  , enemySanityDamage = 3
  , enemyFight = 5
  , enemyHealth = Static 6
  , enemyEvade = 6
  }

instance (IsInvestigator investigator) => HasActions env investigator Umordhoth where
  getActions i window (Umordhoth attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env Umordhoth where
  runMessage msg (Umordhoth attrs) = Umordhoth <$> runMessage msg attrs
