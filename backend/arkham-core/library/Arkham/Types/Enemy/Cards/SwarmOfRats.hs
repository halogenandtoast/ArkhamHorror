{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.SwarmOfRats where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype SwarmOfRats = SwarmOfRats Attrs
  deriving newtype (Show, ToJSON, FromJSON)

swarmOfRats :: EnemyId -> SwarmOfRats
swarmOfRats uuid =
  SwarmOfRats $ baseAttrs uuid "01159" $ (healthDamage .~ 1) . (evade .~ 3)

instance HasModifiersFor env SwarmOfRats where
  getModifiersFor = noModifiersFor

instance HasModifiers env SwarmOfRats where
  getModifiers _ (SwarmOfRats Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env SwarmOfRats where
  getActions i window (SwarmOfRats attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env SwarmOfRats where
  runMessage msg (SwarmOfRats attrs) = SwarmOfRats <$> runMessage msg attrs
