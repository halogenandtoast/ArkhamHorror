module Arkham.Enemy.Cards.ValentinoRivas
  ( valentinoRivas
  , ValentinoRivas(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype ValentinoRivas = ValentinoRivas EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

valentinoRivas :: EnemyCard ValentinoRivas
valentinoRivas = enemy ValentinoRivas Cards.valentinoRivas (3, Static 5, 4) (1, 1)

instance RunMessage ValentinoRivas where
  runMessage msg (ValentinoRivas attrs) =
    ValentinoRivas <$> runMessage msg attrs
