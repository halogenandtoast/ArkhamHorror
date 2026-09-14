module Arkham.Enemy.Cards.NightOfTheZealot.Rats.SwarmOfRats (swarmOfRats) where

import Arkham.Enemy.CardDefs.NightOfTheZealot.Rats qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SwarmOfRats = SwarmOfRats EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

swarmOfRats :: EnemyCard SwarmOfRats
swarmOfRats = enemy SwarmOfRats Cards.swarmOfRats
