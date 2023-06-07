module Arkham.Enemy.Cards.KnightOfTheOuterVoid
  ( knightOfTheOuterVoid
  , KnightOfTheOuterVoid(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype KnightOfTheOuterVoid = KnightOfTheOuterVoid EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

knightOfTheOuterVoid :: EnemyCard KnightOfTheOuterVoid
knightOfTheOuterVoid = enemy KnightOfTheOuterVoid Cards.knightOfTheOuterVoid (3, Static 3, 4) (1, 1)

instance RunMessage KnightOfTheOuterVoid where
  runMessage msg (KnightOfTheOuterVoid attrs) =
    KnightOfTheOuterVoid <$> runMessage msg attrs
