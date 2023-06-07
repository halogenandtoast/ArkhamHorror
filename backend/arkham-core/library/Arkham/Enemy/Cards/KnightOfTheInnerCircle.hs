module Arkham.Enemy.Cards.KnightOfTheInnerCircle
  ( knightOfTheInnerCircle
  , KnightOfTheInnerCircle(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype KnightOfTheInnerCircle = KnightOfTheInnerCircle EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

knightOfTheInnerCircle :: EnemyCard KnightOfTheInnerCircle
knightOfTheInnerCircle = enemy KnightOfTheInnerCircle Cards.knightOfTheInnerCircle (4, Static 4, 2) (2, 0)

instance RunMessage KnightOfTheInnerCircle where
  runMessage msg (KnightOfTheInnerCircle attrs) =
    KnightOfTheInnerCircle <$> runMessage msg attrs
