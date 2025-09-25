module Arkham.Location.Cards.OfficeSpectral (officeSpectral) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.AtDeathsDoorstep.Helpers

newtype OfficeSpectral = OfficeSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

officeSpectral :: LocationCard OfficeSpectral
officeSpectral = location OfficeSpectral Cards.officeSpectral 4 (PerPlayer 2)

instance HasAbilities OfficeSpectral where
  getAbilities (OfficeSpectral a) =
    extendRevealed1 a $ scenarioI18n $ hauntedI "officeSpectral.haunted" a 1

instance RunMessage OfficeSpectral where
  runMessage msg l@(OfficeSpectral attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndDiscardCard iid (attrs.ability 1)
      pure l
    _ -> OfficeSpectral <$> liftRunMessage msg attrs
