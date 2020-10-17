{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.RavenousGhoul where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Prey
import ClassyPrelude

newtype RavenousGhoul = RavenousGhoul Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ravenousGhoul :: EnemyId -> RavenousGhoul
ravenousGhoul uuid = RavenousGhoul $ (baseAttrs uuid "01161")
  { enemyHealthDamage = 1
  , enemySanityDamage = 1
  , enemyFight = 3
  , enemyHealth = Static 3
  , enemyEvade = 3
  , enemyPrey = LowestRemainingHealth
  }

instance HasModifiersFor env RavenousGhoul where
  getModifiersFor _ _ _ = pure []

instance HasModifiers env RavenousGhoul where
  getModifiers _ (RavenousGhoul Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env RavenousGhoul where
  getActions i window (RavenousGhoul attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env RavenousGhoul where
  runMessage msg (RavenousGhoul attrs) = RavenousGhoul <$> runMessage msg attrs
