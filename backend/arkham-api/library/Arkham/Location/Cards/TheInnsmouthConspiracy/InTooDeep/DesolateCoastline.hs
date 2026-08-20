module Arkham.Location.Cards.TheInnsmouthConspiracy.InTooDeep.DesolateCoastline (desolateCoastline) where

import Arkham.Location.CardDefs.TheInnsmouthConspiracy.InTooDeep qualified as Cards
import Arkham.Location.Import.Lifted

newtype DesolateCoastline = DesolateCoastline LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

desolateCoastline :: LocationCard DesolateCoastline
desolateCoastline = locationWith DesolateCoastline Cards.desolateCoastline 2 (Static 1) connectsToAdjacent

instance RunMessage DesolateCoastline where
  runMessage msg (DesolateCoastline attrs) = DesolateCoastline <$> runMessage msg attrs
