module Arkham.Location.Cards.UpstairsDoorwayBedroom
  ( upstairsDoorwayBedroom
  , UpstairsDoorwayBedroom(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype UpstairsDoorwayBedroom = UpstairsDoorwayBedroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

upstairsDoorwayBedroom :: LocationCard UpstairsDoorwayBedroom
upstairsDoorwayBedroom = location UpstairsDoorwayBedroom Cards.upstairsDoorwayBedroom 0 (Static 0)

instance HasAbilities UpstairsDoorwayBedroom where
  getAbilities (UpstairsDoorwayBedroom attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage UpstairsDoorwayBedroom where
  runMessage msg (UpstairsDoorwayBedroom attrs) =
    UpstairsDoorwayBedroom <$> runMessage msg attrs
