module Arkham.Location.Cards.FalconPointApproach (
  falconPointApproach,
  FalconPointApproach (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype FalconPointApproach = FalconPointApproach LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falconPointApproach :: LocationCard FalconPointApproach
falconPointApproach =
  locationWith FalconPointApproach Cards.falconPointApproach 1 (Static 0)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities FalconPointApproach where
  getAbilities (FalconPointApproach attrs) =
    extendRevealed attrs []

instance RunMessage FalconPointApproach where
  runMessage msg (FalconPointApproach attrs) = runQueueT $ case msg of
    _ -> FalconPointApproach <$> liftRunMessage msg attrs
