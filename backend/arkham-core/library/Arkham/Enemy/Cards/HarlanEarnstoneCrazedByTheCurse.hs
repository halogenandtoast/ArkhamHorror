module Arkham.Enemy.Cards.HarlanEarnstoneCrazedByTheCurse
  ( harlanEarnstoneCrazedByTheCurse
  , HarlanEarnstoneCrazedByTheCurse(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype HarlanEarnstoneCrazedByTheCurse = HarlanEarnstoneCrazedByTheCurse EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

harlanEarnstoneCrazedByTheCurse :: EnemyCard HarlanEarnstoneCrazedByTheCurse
harlanEarnstoneCrazedByTheCurse = enemy
  HarlanEarnstoneCrazedByTheCurse
  Cards.harlanEarnstoneCrazedByTheCurse
  (4, Static 2, 3)
  (1, 1)

instance RunMessage HarlanEarnstoneCrazedByTheCurse where
  runMessage msg (HarlanEarnstoneCrazedByTheCurse attrs) =
    HarlanEarnstoneCrazedByTheCurse <$> runMessage msg attrs
