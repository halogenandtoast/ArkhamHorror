module Arkham.Location.Cards.CliffsOfInsanity (cliffsOfInsanity) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CliffsOfInsanity = CliffsOfInsanity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cliffsOfInsanity :: LocationCard CliffsOfInsanity
cliffsOfInsanity =
  symbolLabel
    $ locationWith CliffsOfInsanity Cards.cliffsOfInsanity 3 (PerPlayer 2) connectsToAdjacent

instance HasAbilities CliffsOfInsanity where
  getAbilities (CliffsOfInsanity a) =
    extendRevealed a []

instance RunMessage CliffsOfInsanity where
  runMessage msg (CliffsOfInsanity attrs) = runQueueT $ case msg of
    _ -> CliffsOfInsanity <$> liftRunMessage msg attrs
