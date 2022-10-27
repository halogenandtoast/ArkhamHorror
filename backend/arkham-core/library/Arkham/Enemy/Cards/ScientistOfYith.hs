module Arkham.Enemy.Cards.ScientistOfYith
  ( scientistOfYith
  , ScientistOfYith(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype ScientistOfYith = ScientistOfYith EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

scientistOfYith :: EnemyCard ScientistOfYith
scientistOfYith =
  enemy ScientistOfYith Cards.scientistOfYith (4, Static 3, 1) (2, 0)

instance RunMessage ScientistOfYith where
  runMessage msg (ScientistOfYith attrs) =
    ScientistOfYith <$> runMessage msg attrs
