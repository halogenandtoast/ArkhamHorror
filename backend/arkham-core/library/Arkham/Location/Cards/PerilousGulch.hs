module Arkham.Location.Cards.PerilousGulch
  ( perilousGulch
  , PerilousGulch(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype PerilousGulch = PerilousGulch LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

perilousGulch :: LocationCard PerilousGulch
perilousGulch = location PerilousGulch Cards.perilousGulch 4 (PerPlayer 1)

instance HasAbilities PerilousGulch where
  getAbilities (PerilousGulch attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage PerilousGulch where
  runMessage msg (PerilousGulch attrs) =
    PerilousGulch <$> runMessage msg attrs
