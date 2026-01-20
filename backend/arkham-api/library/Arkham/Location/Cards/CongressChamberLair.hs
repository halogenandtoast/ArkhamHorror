module Arkham.Location.Cards.CongressChamberLair (congressChamberLair) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Coterie))

newtype CongressChamberLair = CongressChamberLair LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

congressChamberLair :: LocationCard CongressChamberLair
congressChamberLair = location CongressChamberLair Cards.congressChamberLair 3 (PerPlayer 1)

instance HasModifiersFor CongressChamberLair where
  getModifiersFor (CongressChamberLair a) = do
    whenAny (ReadyEnemy <> EnemyWithTrait Coterie) do
      modifySelect a (InvestigatorAt $ be a) [IncreaseCostOf #asset 2]
