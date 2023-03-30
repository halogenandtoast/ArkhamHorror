module Arkham.Enemy.Cards.Wraith
  ( wraith
  , Wraith(..)
  )
where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype Wraith = Wraith EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

wraith :: EnemyCard Wraith
wraith = enemy Wraith Cards.wraith (2, Static 2, 2) (0, 2)

instance RunMessage Wraith where
  runMessage msg (Wraith attrs) =
    Wraith <$> runMessage msg attrs
