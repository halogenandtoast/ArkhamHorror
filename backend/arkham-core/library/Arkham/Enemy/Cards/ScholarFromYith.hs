module Arkham.Enemy.Cards.ScholarFromYith
  ( scholarFromYith
  , ScholarFromYith(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype ScholarFromYith = ScholarFromYith EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

scholarFromYith :: EnemyCard ScholarFromYith
scholarFromYith =
  enemy ScholarFromYith Cards.scholarFromYith (2, Static 2, 2) (0, 1)

instance RunMessage ScholarFromYith where
  runMessage msg (ScholarFromYith attrs) =
    ScholarFromYith <$> runMessage msg attrs
