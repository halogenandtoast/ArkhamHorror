module Arkham.Enemy.Cards.FormlessSpawn
  ( formlessSpawn
  , FormlessSpawn(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype FormlessSpawn = FormlessSpawn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

formlessSpawn :: EnemyCard FormlessSpawn
formlessSpawn =
  enemy FormlessSpawn Cards.formlessSpawn (2, Static 10, 2) (3, 3)

instance RunMessage FormlessSpawn where
  runMessage msg (FormlessSpawn attrs) = FormlessSpawn <$> runMessage msg attrs
