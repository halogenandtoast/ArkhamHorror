module Arkham.Location.Cards.BilliardsRoomSpectral
  ( billiardsRoomSpectral
  , BilliardsRoomSpectral(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype BilliardsRoomSpectral = BilliardsRoomSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

billiardsRoomSpectral :: LocationCard BilliardsRoomSpectral
billiardsRoomSpectral = location BilliardsRoomSpectral Cards.billiardsRoomSpectral 3 (PerPlayer 1)

instance HasAbilities BilliardsRoomSpectral where
  getAbilities (BilliardsRoomSpectral attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BilliardsRoomSpectral where
  runMessage msg (BilliardsRoomSpectral attrs) =
    BilliardsRoomSpectral <$> runMessage msg attrs
