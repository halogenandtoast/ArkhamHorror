{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.YoungDeepOne where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import ClassyPrelude

newtype YoungDeepOne = YoungDeepOne Attrs
  deriving newtype (Show, ToJSON, FromJSON)

youngDeepOne :: EnemyId -> YoungDeepOne
youngDeepOne uuid = YoungDeepOne $ (baseAttrs uuid "01181")
  { enemyHealthDamage = 1
  , enemySanityDamage = 1
  , enemyFight = 3
  , enemyHealth = Static 3
  , enemyEvade = 3
  }

instance (IsInvestigator investigator) => HasActions env investigator YoungDeepOne where
  getActions i window (YoungDeepOne attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env YoungDeepOne where
  runMessage msg (YoungDeepOne attrs) = YoungDeepOne <$> runMessage msg attrs
