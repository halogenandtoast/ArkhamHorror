module Arkham.Enemy.Cards.NathanWickMasterOfIndoctrination
  ( nathanWickMasterOfIndoctrination
  , NathanWickMasterOfIndoctrination(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype NathanWickMasterOfIndoctrination = NathanWickMasterOfIndoctrination EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nathanWickMasterOfIndoctrination :: EnemyCard NathanWickMasterOfIndoctrination
nathanWickMasterOfIndoctrination = enemy NathanWickMasterOfIndoctrination Cards.nathanWickMasterOfIndoctrination (4, Static 5, 3) (1, 1)

instance RunMessage NathanWickMasterOfIndoctrination where
  runMessage msg (NathanWickMasterOfIndoctrination attrs) =
    NathanWickMasterOfIndoctrination <$> runMessage msg attrs
