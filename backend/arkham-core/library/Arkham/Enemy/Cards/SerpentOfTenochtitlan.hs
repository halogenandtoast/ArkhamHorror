module Arkham.Enemy.Cards.SerpentOfTenochtitlan
  ( serpentOfTenochtitlan
  , SerpentOfTenochtitlan(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype SerpentOfTenochtitlan = SerpentOfTenochtitlan EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

serpentOfTenochtitlan :: EnemyCard SerpentOfTenochtitlan
serpentOfTenochtitlan = enemy
  SerpentOfTenochtitlan
  Cards.serpentOfTenochtitlan
  (3, Static 5, 3)
  (1, 1)

instance RunMessage SerpentOfTenochtitlan where
  runMessage msg (SerpentOfTenochtitlan attrs) =
    SerpentOfTenochtitlan <$> runMessage msg attrs
