module Arkham.Enemy.Cards.BroodSoldier (broodSoldier) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher

newtype BroodSoldier = BroodSoldier EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

broodSoldier :: EnemyCard BroodSoldier
broodSoldier = enemy BroodSoldier Cards.broodSoldier (3, Static 2, 3) (1, 1)

instance HasModifiersFor BroodSoldier where
  getModifiersFor (BroodSoldier a) = do
    when a.ready $ modifySelect a (locationWithEnemy a) [ShroudModifier 2]
