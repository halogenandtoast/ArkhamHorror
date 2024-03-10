module Arkham.Enemy.Cards.MoonLizard
  ( moonLizard
  , MoonLizard(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype MoonLizard = MoonLizard EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moonLizard :: EnemyCard MoonLizard
moonLizard = enemy MoonLizard Cards.moonLizard (0, Static 1, 0) (0, 0)

instance RunMessage MoonLizard where
  runMessage msg (MoonLizard attrs) =
    MoonLizard <$> runMessage msg attrs
