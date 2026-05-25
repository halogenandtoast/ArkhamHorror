module Arkham.Location.Cards.TangledThicket (tangledThicket) where

import Arkham.Card
import Arkham.Cost
import Arkham.Helpers.Location (swapLocation)
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted

newtype TangledThicket = TangledThicket LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tangledThicket :: LocationCard TangledThicket
tangledThicket = locationWith TangledThicket Cards.tangledThicket 2 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor TangledThicket where
  getModifiersFor (TangledThicket a) =
    modifySelf a [AdditionalCostToLeave $ ActionCost 1]

instance HasAbilities TangledThicket where
  getAbilities (TangledThicket a) = extendRevealed a []

instance RunMessage TangledThicket where
  runMessage msg l@(TangledThicket attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.openWater10594b
      pure l
    _ -> TangledThicket <$> liftRunMessage msg attrs
