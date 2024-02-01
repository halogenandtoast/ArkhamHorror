module Arkham.Location.Cards.CityOfTheSerpents (
  cityOfTheSerpents,
  CityOfTheSerpents (..),
) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Projection

newtype CityOfTheSerpents = CityOfTheSerpents LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

cityOfTheSerpents :: LocationCard CityOfTheSerpents
cityOfTheSerpents =
  symbolLabel
    $ location CityOfTheSerpents Cards.cityOfTheSerpents 3 (PerPlayer 1)

instance HasModifiersFor CityOfTheSerpents where
  getModifiersFor target (CityOfTheSerpents a) | isTarget a target = do
    clueless <- fieldMap LocationClues (== 0) (toId a)
    pure $ toModifiers a [InVictoryDisplayForCountingVengeance | clueless]
  getModifiersFor _ _ = pure []

instance RunMessage CityOfTheSerpents where
  runMessage msg (CityOfTheSerpents attrs) =
    CityOfTheSerpents <$> runMessage msg attrs
