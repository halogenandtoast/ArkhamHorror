module Arkham.Types.Enemy.Cards.RavenousGhoul
  ( ravenousGhoul
  , RavenousGhoul(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Prey

newtype RavenousGhoul = RavenousGhoul EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ravenousGhoul :: EnemyCard RavenousGhoul
ravenousGhoul = enemyWith
  RavenousGhoul
  Cards.ravenousGhoul
  (3, Static 3, 3)
  (1, 1)
  (preyL .~ LowestRemainingHealth)

instance HasModifiersFor env RavenousGhoul where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env RavenousGhoul where
  getActions i window (RavenousGhoul attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env RavenousGhoul where
  runMessage msg (RavenousGhoul attrs) = RavenousGhoul <$> runMessage msg attrs
