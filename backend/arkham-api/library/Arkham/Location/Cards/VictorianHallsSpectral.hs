module Arkham.Location.Cards.VictorianHallsSpectral (victorianHallsSpectral) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype VictorianHallsSpectral = VictorianHallsSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

victorianHallsSpectral :: LocationCard VictorianHallsSpectral
victorianHallsSpectral = location VictorianHallsSpectral Cards.victorianHallsSpectral 4 (Static 0)

instance HasAbilities VictorianHallsSpectral where
  getAbilities (VictorianHallsSpectral a) =
    extendRevealed1 a $ withI18n $ countVar 1 $ hauntedI "loseActions" a 1

instance RunMessage VictorianHallsSpectral where
  runMessage msg l@(VictorianHallsSpectral attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseActions iid (attrs.ability 1) 1
      pure l
    _ -> VictorianHallsSpectral <$> liftRunMessage msg attrs
