module Arkham.Enemy.Cards.ApexStrangleweed
  ( apexStrangleweed
  , ApexStrangleweed(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype ApexStrangleweed = ApexStrangleweed EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

apexStrangleweed :: EnemyCard ApexStrangleweed
apexStrangleweed =
  enemy ApexStrangleweed Cards.apexStrangleweed (3, Static 6, 3) (1, 1)

instance RunMessage ApexStrangleweed where
  runMessage msg (ApexStrangleweed attrs) =
    ApexStrangleweed <$> runMessage msg attrs
