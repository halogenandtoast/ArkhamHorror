module Arkham.Enemy.Cards.HouseDealerA (houseDealerA) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher

newtype HouseDealerA = HouseDealerA EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

houseDealerA :: EnemyCard HouseDealerA
houseDealerA = enemy HouseDealerA Cards.houseDealerA (2, Static 1, 2) (0, 1)

instance HasModifiersFor HouseDealerA where
  getModifiersFor (HouseDealerA a) = do
    abilities <- select $ AbilityOnLocation (locationWithEnemy a)
    eachInvestigator \iid -> do
      for_ abilities \ab ->
        modified_ a (AbilityTarget iid ab.ref) [ActionCostModifier 1]

instance RunMessage HouseDealerA where
  runMessage msg (HouseDealerA attrs) = HouseDealerA <$> runMessage msg attrs
