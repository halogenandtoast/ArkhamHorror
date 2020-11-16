{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.RavenousGhoul where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype RavenousGhoul = RavenousGhoul Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ravenousGhoul :: EnemyId -> RavenousGhoul
ravenousGhoul uuid =
  RavenousGhoul
    $ baseAttrs uuid "01161"
    $ (healthDamage .~ 1)
    . (sanityDamage .~ 1)
    . (fight .~ 3)
    . (health .~ Static 3)
    . (evade .~ 3)
    . (prey .~ LowestRemainingHealth)

instance HasModifiersFor env RavenousGhoul where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env RavenousGhoul where
  getActions i window (RavenousGhoul attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env RavenousGhoul where
  runMessage msg (RavenousGhoul attrs) = RavenousGhoul <$> runMessage msg attrs
