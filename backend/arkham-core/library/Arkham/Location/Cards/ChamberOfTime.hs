module Arkham.Location.Cards.ChamberOfTime
  ( chamberOfTime
  , ChamberOfTime(..)
  ) where

import Arkham.Prelude

import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ChamberOfTime = ChamberOfTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfTime :: LocationCard ChamberOfTime
chamberOfTime = locationWith
  ChamberOfTime
  Cards.chamberOfTime
  4
  (PerPlayer 2)
  (connectsToL .~ singleton RightOf)

instance HasAbilities ChamberOfTime where
  getAbilities (ChamberOfTime attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage ChamberOfTime where
  runMessage msg (ChamberOfTime attrs) = ChamberOfTime <$> runMessage msg attrs
