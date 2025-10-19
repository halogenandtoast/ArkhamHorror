module Arkham.Location.Cards.IstanbulUniversity (istanbulUniversity) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype IstanbulUniversity = IstanbulUniversity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

istanbulUniversity :: LocationCard IstanbulUniversity
istanbulUniversity = symbolLabel $ location IstanbulUniversity Cards.istanbulUniversity 4 (PerPlayer 1)

instance HasAbilities IstanbulUniversity where
  getAbilities (IstanbulUniversity attrs) =
    extendRevealed attrs []

instance RunMessage IstanbulUniversity where
  runMessage msg (IstanbulUniversity attrs) = runQueueT $ case msg of
    _ -> IstanbulUniversity <$> liftRunMessage msg attrs
