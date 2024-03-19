module Arkham.Location.Cards.OnyxGates
  ( onyxGates
  , OnyxGates(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OnyxGates = OnyxGates LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onyxGates :: LocationCard OnyxGates
onyxGates = location OnyxGates Cards.onyxGates 1 (Static 12)

instance HasAbilities OnyxGates where
  getAbilities (OnyxGates attrs) =
    extendRevealed attrs []

instance RunMessage OnyxGates where
  runMessage msg (OnyxGates attrs) = runQueueT $ case msg of
    _ -> OnyxGates <$> lift (runMessage msg attrs)
