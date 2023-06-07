module Arkham.Enemy.Cards.NathanWickMasterOfInitiation
  ( nathanWickMasterOfInitiation
  , NathanWickMasterOfInitiation(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype NathanWickMasterOfInitiation = NathanWickMasterOfInitiation EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nathanWickMasterOfInitiation :: EnemyCard NathanWickMasterOfInitiation
nathanWickMasterOfInitiation = enemy NathanWickMasterOfInitiation Cards.nathanWickMasterOfInitiation (3, Static 5, 4) (1, 1)

instance RunMessage NathanWickMasterOfInitiation where
  runMessage msg (NathanWickMasterOfInitiation attrs) =
    NathanWickMasterOfInitiation <$> runMessage msg attrs
