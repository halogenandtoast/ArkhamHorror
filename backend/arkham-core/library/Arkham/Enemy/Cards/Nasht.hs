module Arkham.Enemy.Cards.Nasht
  ( nasht
  , Nasht(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype Nasht = Nasht EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nasht :: EnemyCard Nasht
nasht = enemy Nasht Cards.nasht (2, Static 3, 2) (0, 1)

instance RunMessage Nasht where
  runMessage msg (Nasht attrs) =
    Nasht <$> runMessage msg attrs
