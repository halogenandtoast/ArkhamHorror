module Arkham.Location.Cards.SawboneAlleyInTooDeep (
  sawboneAlleyInTooDeep,
  SawboneAlleyInTooDeep (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted

newtype SawboneAlleyInTooDeep = SawboneAlleyInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sawboneAlleyInTooDeep :: LocationCard SawboneAlleyInTooDeep
sawboneAlleyInTooDeep = locationWith SawboneAlleyInTooDeep Cards.sawboneAlleyInTooDeep 4 (PerPlayer 2) connectsToAdjacent

instance HasAbilities SawboneAlleyInTooDeep where
  getAbilities (SawboneAlleyInTooDeep attrs) =
    extendRevealed attrs []

instance RunMessage SawboneAlleyInTooDeep where
  runMessage msg (SawboneAlleyInTooDeep attrs) = runQueueT $ case msg of
    _ -> SawboneAlleyInTooDeep <$> liftRunMessage msg attrs
