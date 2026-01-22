module Arkham.Location.Cards.IridescentPassage (iridescentPassage) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype IridescentPassage = IridescentPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iridescentPassage :: LocationCard IridescentPassage
iridescentPassage = locationWith IridescentPassage Cards.iridescentPassage 3 (PerPlayer 2) connectsToAdjacent

instance HasAbilities IridescentPassage where
  getAbilities (IridescentPassage a) =
    extendRevealed a []

instance RunMessage IridescentPassage where
  runMessage msg (IridescentPassage attrs) = runQueueT $ case msg of
    _ -> IridescentPassage <$> liftRunMessage msg attrs
