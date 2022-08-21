module Arkham.Location.Cards.GrandChamber
  ( grandChamber
  , GrandChamber(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype GrandChamber = GrandChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandChamber :: LocationCard GrandChamber
grandChamber = location GrandChamber Cards.grandChamber 2 (PerPlayer 1)

instance HasAbilities GrandChamber where
  getAbilities (GrandChamber attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage GrandChamber where
  runMessage msg (GrandChamber attrs) =
    GrandChamber <$> runMessage msg attrs
