module Arkham.Location.Cards.EsotericOrderOfDagonInTooDeep (
  esotericOrderOfDagonInTooDeep,
  EsotericOrderOfDagonInTooDeep (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted

newtype EsotericOrderOfDagonInTooDeep = EsotericOrderOfDagonInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericOrderOfDagonInTooDeep :: LocationCard EsotericOrderOfDagonInTooDeep
esotericOrderOfDagonInTooDeep =
  locationWith
    EsotericOrderOfDagonInTooDeep
    Cards.esotericOrderOfDagonInTooDeep
    4
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities EsotericOrderOfDagonInTooDeep where
  getAbilities (EsotericOrderOfDagonInTooDeep attrs) =
    extendRevealed attrs []

instance RunMessage EsotericOrderOfDagonInTooDeep where
  runMessage msg (EsotericOrderOfDagonInTooDeep attrs) = runQueueT $ case msg of
    _ -> EsotericOrderOfDagonInTooDeep <$> liftRunMessage msg attrs
