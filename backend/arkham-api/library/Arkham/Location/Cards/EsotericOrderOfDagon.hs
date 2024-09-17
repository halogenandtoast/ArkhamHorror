module Arkham.Location.Cards.EsotericOrderOfDagon
  ( esotericOrderOfDagon
  , EsotericOrderOfDagon(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype EsotericOrderOfDagon = EsotericOrderOfDagon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericOrderOfDagon :: LocationCard EsotericOrderOfDagon
esotericOrderOfDagon = location EsotericOrderOfDagon Cards.esotericOrderOfDagon 3 (PerPlayer 1)

instance HasAbilities EsotericOrderOfDagon where
  getAbilities (EsotericOrderOfDagon attrs) =
    extendRevealed attrs []

instance RunMessage EsotericOrderOfDagon where
  runMessage msg (EsotericOrderOfDagon attrs) = runQueueT $ case msg of
    _ -> EsotericOrderOfDagon <$> liftRunMessage msg attrs
