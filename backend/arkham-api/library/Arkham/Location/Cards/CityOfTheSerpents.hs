module Arkham.Location.Cards.CityOfTheSerpents (cityOfTheSerpents, CityOfTheSerpents (..)) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Projection

newtype CityOfTheSerpents = CityOfTheSerpents LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cityOfTheSerpents :: LocationCard CityOfTheSerpents
cityOfTheSerpents =
  symbolLabel $ location CityOfTheSerpents Cards.cityOfTheSerpents 3 (PerPlayer 1)

instance HasModifiersFor CityOfTheSerpents where
  getModifiersFor (CityOfTheSerpents a) = whenRevealed a $ maybeModifySelf a do
    liftGuardM $ fieldMap LocationClues (== 0) a.id
    pure [InVictoryDisplayForCountingVengeance]

instance RunMessage CityOfTheSerpents where
  runMessage msg (CityOfTheSerpents attrs) = CityOfTheSerpents <$> runMessage msg attrs
