module Arkham.Location.Cards.VictorianHallsSpectral (
  victorianHallsSpectral,
  VictorianHallsSpectral (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype VictorianHallsSpectral = VictorianHallsSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

victorianHallsSpectral :: LocationCard VictorianHallsSpectral
victorianHallsSpectral =
  location VictorianHallsSpectral Cards.victorianHallsSpectral 4 (Static 0)

instance HasAbilities VictorianHallsSpectral where
  getAbilities (VictorianHallsSpectral attrs) =
    withBaseAbilities attrs [haunted "Lost 1 action." attrs 1]

instance RunMessage VictorianHallsSpectral where
  runMessage msg l@(VictorianHallsSpectral attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ LoseActions iid (toSource attrs) 1
      pure l
    _ -> VictorianHallsSpectral <$> runMessage msg attrs
