module Arkham.Location.Cards.DownstairsDoorwayDen
  ( downstairsDoorwayDen
  , DownstairsDoorwayDen(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype DownstairsDoorwayDen = DownstairsDoorwayDen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downstairsDoorwayDen :: LocationCard DownstairsDoorwayDen
downstairsDoorwayDen = location DownstairsDoorwayDen Cards.downstairsDoorwayDen 0 (Static 0)

instance HasAbilities DownstairsDoorwayDen where
  getAbilities (DownstairsDoorwayDen attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage DownstairsDoorwayDen where
  runMessage msg (DownstairsDoorwayDen attrs) =
    DownstairsDoorwayDen <$> runMessage msg attrs
