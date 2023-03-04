module Arkham.Location.Cards.TrophyRoomSpectral
  ( trophyRoomSpectral
  , TrophyRoomSpectral(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype TrophyRoomSpectral = TrophyRoomSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trophyRoomSpectral :: LocationCard TrophyRoomSpectral
trophyRoomSpectral = location TrophyRoomSpectral Cards.trophyRoomSpectral 2 (PerPlayer 1)

instance HasAbilities TrophyRoomSpectral where
  getAbilities (TrophyRoomSpectral attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage TrophyRoomSpectral where
  runMessage msg (TrophyRoomSpectral attrs) =
    TrophyRoomSpectral <$> runMessage msg attrs
