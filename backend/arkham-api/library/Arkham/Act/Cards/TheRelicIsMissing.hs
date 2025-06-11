module Arkham.Act.Cards.TheRelicIsMissing (theRelicIsMissing) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Location.Cards qualified as Locations

newtype TheRelicIsMissing = TheRelicIsMissing ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theRelicIsMissing :: ActCard TheRelicIsMissing
theRelicIsMissing =
  act (1, A) TheRelicIsMissing Cards.theRelicIsMissing
    $ Just
    $ GroupClueCost (PerPlayer 1) "Miskatonic University"

instance RunMessage TheRelicIsMissing where
  runMessage msg a@(TheRelicIsMissing attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      placeLocation_ Locations.eztliExhibit
      advanceActDeck attrs
      pure a
    _ -> TheRelicIsMissing <$> liftRunMessage msg attrs
