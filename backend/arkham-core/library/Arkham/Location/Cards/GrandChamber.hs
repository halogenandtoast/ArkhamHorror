module Arkham.Location.Cards.GrandChamber
  ( grandChamber
  , GrandChamber(..)
  ) where

import Arkham.Prelude

import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype GrandChamber = GrandChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandChamber :: LocationCard GrandChamber
grandChamber = locationWith
  GrandChamber
  Cards.grandChamber
  2
  (PerPlayer 1)
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasAbilities GrandChamber where
  getAbilities (GrandChamber attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage GrandChamber where
  runMessage msg (GrandChamber attrs) = GrandChamber <$> runMessage msg attrs
