module Arkham.Enemy.Cards.Azathoth
  ( azathoth
  , Azathoth(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype Azathoth = Azathoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

azathoth :: EnemyCard Azathoth
azathoth = enemy Azathoth Cards.azathoth (0, Static 1, 0) (0, 0)

instance RunMessage Azathoth where
  runMessage msg (Azathoth attrs) =
    Azathoth <$> runMessage msg attrs
