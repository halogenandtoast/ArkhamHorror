module Arkham.Types.Enemy.Cards.SwarmOfRats where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype SwarmOfRats = SwarmOfRats EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swarmOfRats :: EnemyId -> SwarmOfRats
swarmOfRats uuid =
  SwarmOfRats $ baseAttrs uuid "01159" $ (healthDamageL .~ 1) . (evadeL .~ 3)

instance HasModifiersFor env SwarmOfRats where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env SwarmOfRats where
  getActions i window (SwarmOfRats attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env SwarmOfRats where
  runMessage msg (SwarmOfRats attrs) = SwarmOfRats <$> runMessage msg attrs
