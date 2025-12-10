module Arkham.Location.Cards.OutsidersLairWithoutATrace (outsidersLairWithoutATrace) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OutsidersLairWithoutATrace = OutsidersLairWithoutATrace LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outsidersLairWithoutATrace :: LocationCard OutsidersLairWithoutATrace
outsidersLairWithoutATrace =
  symbolLabel
    $ locationWith
      OutsidersLairWithoutATrace
      Cards.outsidersLairWithoutATrace
      5
      (PerPlayer 2)
      connectsToAdjacent

instance HasAbilities OutsidersLairWithoutATrace where
  getAbilities (OutsidersLairWithoutATrace a) =
    extendRevealed a []

instance RunMessage OutsidersLairWithoutATrace where
  runMessage msg (OutsidersLairWithoutATrace attrs) = runQueueT $ case msg of
    _ -> OutsidersLairWithoutATrace <$> liftRunMessage msg attrs
