module Arkham.Location.Cards.TraitorsGate (traitorsGate) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TraitorsGate = TraitorsGate LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

traitorsGate :: LocationCard TraitorsGate
traitorsGate = symbolLabel $ location TraitorsGate Cards.traitorsGate 4 (PerPlayer 1)

instance HasAbilities TraitorsGate where
  getAbilities (TraitorsGate attrs) =
    extendRevealed attrs []

instance RunMessage TraitorsGate where
  runMessage msg (TraitorsGate attrs) = runQueueT $ case msg of
    _ -> TraitorsGate <$> liftRunMessage msg attrs
