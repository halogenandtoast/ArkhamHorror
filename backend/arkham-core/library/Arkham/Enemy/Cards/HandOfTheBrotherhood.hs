module Arkham.Enemy.Cards.HandOfTheBrotherhood
  ( handOfTheBrotherhood
  , HandOfTheBrotherhood(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype HandOfTheBrotherhood = HandOfTheBrotherhood EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

handOfTheBrotherhood :: EnemyCard HandOfTheBrotherhood
handOfTheBrotherhood =
  enemy HandOfTheBrotherhood Cards.handOfTheBrotherhood (2, Static 2, 2) (0, 1)

instance RunMessage HandOfTheBrotherhood where
  runMessage msg (HandOfTheBrotherhood attrs) =
    HandOfTheBrotherhood <$> runMessage msg attrs
