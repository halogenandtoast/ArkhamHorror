{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.YithianObserver where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import ClassyPrelude

newtype YithianObserver = YithianObserver Attrs
  deriving newtype (Show, ToJSON, FromJSON)

yithianObserver :: EnemyId -> YithianObserver
yithianObserver uuid = YithianObserver $ (baseAttrs uuid "01177")
  { enemyHealthDamage = 1
  , enemySanityDamage = 1
  , enemyFight = 4
  , enemyHealth = Static 4
  , enemyEvade = 3
  }

instance (IsInvestigator investigator) => HasActions env investigator YithianObserver where
  getActions i window (YithianObserver attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env YithianObserver where
  runMessage msg (YithianObserver attrs) =
    YithianObserver <$> runMessage msg attrs
