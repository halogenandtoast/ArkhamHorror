module Arkham.Location.Cards.FalconPointGatehouse (falconPointGatehouse, FalconPointGatehouse (..)) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.ALightInTheFog.Helpers.Location

newtype FalconPointGatehouse = FalconPointGatehouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falconPointGatehouse :: LocationCard FalconPointGatehouse
falconPointGatehouse = location FalconPointGatehouse Cards.falconPointGatehouse 1 (Static 0)

instance HasAbilities FalconPointGatehouse where
  getAbilities (FalconPointGatehouse a) =
    extendRevealed
      a
      [ withTooltip "You head back into the woods, leaving the lighthouse and its mysteries behind."
          $ resignAction a
      ]

instance RunMessage FalconPointGatehouse where
  runMessage msg l@(FalconPointGatehouse attrs) = runQueueT $ case msg of
    PlaceGrid (GridLocation pos lid) | lid == attrs.id -> setConnectedInRow pos l
    _ -> FalconPointGatehouse <$> liftRunMessage msg attrs
