module Arkham.Location.Cards.ChamberOfTime
  ( chamberOfTime
  , ChamberOfTime(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype ChamberOfTime = ChamberOfTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfTime :: LocationCard ChamberOfTime
chamberOfTime = location ChamberOfTime Cards.chamberOfTime 4 (PerPlayer 2)

instance HasAbilities ChamberOfTime where
  getAbilities (ChamberOfTime attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage ChamberOfTime where
  runMessage msg (ChamberOfTime attrs) =
    ChamberOfTime <$> runMessage msg attrs
