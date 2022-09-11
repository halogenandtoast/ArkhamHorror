module Arkham.Location.Cards.VelmasDiner
  ( velmasDiner
  , VelmasDiner(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype VelmasDiner = VelmasDiner LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

velmasDiner :: LocationCard VelmasDiner
velmasDiner = location VelmasDiner Cards.velmasDiner 2 (Static 0)

instance HasAbilities VelmasDiner where
  getAbilities (VelmasDiner attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage VelmasDiner where
  runMessage msg (VelmasDiner attrs) =
    VelmasDiner <$> runMessage msg attrs
