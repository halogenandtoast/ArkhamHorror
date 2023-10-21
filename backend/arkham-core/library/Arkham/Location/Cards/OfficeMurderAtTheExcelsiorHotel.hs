module Arkham.Location.Cards.OfficeMurderAtTheExcelsiorHotel
  ( officeMurderAtTheExcelsiorHotel
  , OfficeMurderAtTheExcelsiorHotel(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype OfficeMurderAtTheExcelsiorHotel = OfficeMurderAtTheExcelsiorHotel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

officeMurderAtTheExcelsiorHotel :: LocationCard OfficeMurderAtTheExcelsiorHotel
officeMurderAtTheExcelsiorHotel = location OfficeMurderAtTheExcelsiorHotel Cards.officeMurderAtTheExcelsiorHotel 3 (PerPlayer 2)

instance HasAbilities OfficeMurderAtTheExcelsiorHotel where
  getAbilities (OfficeMurderAtTheExcelsiorHotel attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage OfficeMurderAtTheExcelsiorHotel where
  runMessage msg (OfficeMurderAtTheExcelsiorHotel attrs) =
    OfficeMurderAtTheExcelsiorHotel <$> runMessage msg attrs
