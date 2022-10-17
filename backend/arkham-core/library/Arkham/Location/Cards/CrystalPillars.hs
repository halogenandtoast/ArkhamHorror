module Arkham.Location.Cards.CrystalPillars
  ( crystalPillars
  , CrystalPillars(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype CrystalPillars = CrystalPillars LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalPillars :: LocationCard CrystalPillars
crystalPillars = location CrystalPillars Cards.crystalPillars 1 (PerPlayer 2)

instance HasAbilities CrystalPillars where
  getAbilities (CrystalPillars attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage CrystalPillars where
  runMessage msg (CrystalPillars attrs) =
    CrystalPillars <$> runMessage msg attrs
