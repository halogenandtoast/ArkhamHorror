module Arkham.Location.Cards.LodgeCatacombs
  ( lodgeCatacombs
  , LodgeCatacombs(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype LodgeCatacombs = LodgeCatacombs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeCatacombs :: LocationCard LodgeCatacombs
lodgeCatacombs = location LodgeCatacombs Cards.lodgeCatacombs 4 (Static 0)

instance HasAbilities LodgeCatacombs where
  getAbilities (LodgeCatacombs attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage LodgeCatacombs where
  runMessage msg (LodgeCatacombs attrs) =
    LodgeCatacombs <$> runMessage msg attrs
