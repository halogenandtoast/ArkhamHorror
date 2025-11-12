module Arkham.Enemy.Cards.HouseDealerB (houseDealerB) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher

newtype HouseDealerB = HouseDealerB EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

houseDealerB :: EnemyCard HouseDealerB
houseDealerB = enemy HouseDealerB Cards.houseDealerB (2, Static 1, 2) (0, 1)

instance HasModifiersFor HouseDealerB where
  getModifiersFor (HouseDealerB a) = do
    abilities <- select $ AbilityOnLocation (locationWithEnemy a)
    eachInvestigator \iid -> do
      for_ abilities \ab ->
        modified_ a (AbilityTarget iid ab.ref) [ActionCostModifier 1]

instance RunMessage HouseDealerB where
  runMessage msg (HouseDealerB attrs) = HouseDealerB <$> runMessage msg attrs
