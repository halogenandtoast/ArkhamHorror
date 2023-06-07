module Arkham.Location.Cards.SanctumDoorwayHoldingCells
  ( sanctumDoorwayHoldingCells
  , SanctumDoorwayHoldingCells(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SanctumDoorwayHoldingCells = SanctumDoorwayHoldingCells LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanctumDoorwayHoldingCells :: LocationCard SanctumDoorwayHoldingCells
sanctumDoorwayHoldingCells = location SanctumDoorwayHoldingCells Cards.sanctumDoorwayHoldingCells 1 (PerPlayer 1)

instance HasAbilities SanctumDoorwayHoldingCells where
  getAbilities (SanctumDoorwayHoldingCells attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage SanctumDoorwayHoldingCells where
  runMessage msg (SanctumDoorwayHoldingCells attrs) =
    SanctumDoorwayHoldingCells <$> runMessage msg attrs
