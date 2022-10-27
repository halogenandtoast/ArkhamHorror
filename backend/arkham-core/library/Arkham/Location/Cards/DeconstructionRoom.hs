module Arkham.Location.Cards.DeconstructionRoom
  ( deconstructionRoom
  , DeconstructionRoom(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype DeconstructionRoom = DeconstructionRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deconstructionRoom :: LocationCard DeconstructionRoom
deconstructionRoom =
  location DeconstructionRoom Cards.deconstructionRoom 3 (PerPlayer 1)

instance HasAbilities DeconstructionRoom where
  getAbilities (DeconstructionRoom attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage DeconstructionRoom where
  runMessage msg (DeconstructionRoom attrs) =
    DeconstructionRoom <$> runMessage msg attrs
