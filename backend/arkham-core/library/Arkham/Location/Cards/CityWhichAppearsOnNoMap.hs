module Arkham.Location.Cards.CityWhichAppearsOnNoMap
  ( cityWhichAppearsOnNoMap
  , CityWhichAppearsOnNoMap(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype CityWhichAppearsOnNoMap = CityWhichAppearsOnNoMap LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityWhichAppearsOnNoMap :: LocationCard CityWhichAppearsOnNoMap
cityWhichAppearsOnNoMap = location CityWhichAppearsOnNoMap Cards.cityWhichAppearsOnNoMap 6 (PerPlayer 2)

instance HasAbilities CityWhichAppearsOnNoMap where
  getAbilities (CityWhichAppearsOnNoMap attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage CityWhichAppearsOnNoMap where
  runMessage msg (CityWhichAppearsOnNoMap attrs) =
    CityWhichAppearsOnNoMap <$> runMessage msg attrs
