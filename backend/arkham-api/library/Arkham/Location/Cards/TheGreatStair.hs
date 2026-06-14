module Arkham.Location.Cards.TheGreatStair (theGreatStair) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheGrandVault.Helpers

newtype TheGreatStair = TheGreatStair LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatStair :: LocationCard TheGreatStair
theGreatStair = location TheGreatStair Cards.theGreatStair 2 (Static 2)

instance HasModifiersFor TheGreatStair where
  getModifiersFor (TheGreatStair a) = modifySelf a [CannotBeFlooded]

instance HasAbilities TheGreatStair where
  getAbilities (TheGreatStair a) =
    extendRevealed1 a $ scenarioI18n $ withI18nTooltip "theGreatStair.resign" $ locationResignAction a

instance RunMessage TheGreatStair where
  runMessage msg (TheGreatStair attrs) = runQueueT $ TheGreatStair <$> liftRunMessage msg attrs
