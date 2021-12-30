module Arkham.Enemy.Cards.TheOrganistDrapedInMystery
  ( theOrganistDrapedInMystery
  , TheOrganistDrapedInMystery(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Attrs

newtype TheOrganistDrapedInMystery = TheOrganistDrapedInMystery EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theOrganistDrapedInMystery :: EnemyCard TheOrganistDrapedInMystery
theOrganistDrapedInMystery = enemy
  TheOrganistDrapedInMystery
  Cards.theOrganistDrapedInMystery
  (3, Static 1, 5)
  (0, 1)

instance EnemyRunner env => RunMessage env TheOrganistDrapedInMystery where
  runMessage msg (TheOrganistDrapedInMystery attrs) =
    TheOrganistDrapedInMystery <$> runMessage msg attrs
