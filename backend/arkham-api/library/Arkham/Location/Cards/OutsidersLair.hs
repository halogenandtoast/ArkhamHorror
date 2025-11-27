module Arkham.Location.Cards.OutsidersLair (outsidersLair) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype OutsidersLair = OutsidersLair LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

outsidersLair :: LocationCard OutsidersLair
outsidersLair = location OutsidersLair Cards.outsidersLair 4 (PerPlayer 2)

instance HasModifiersFor OutsidersLair where
  getModifiersFor (OutsidersLair a) = do
    modifySelectWhen
      a
      (a.clues == 0)
      (enemyIs Enemies.voidChimeraTrueForm)
      [EnemyFight (-1), EnemyEvade (-1)]
