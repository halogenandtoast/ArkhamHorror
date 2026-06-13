module Arkham.Location.Cards.SunkenStairway (sunkenStairway) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SunkenStairway = SunkenStairway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenStairway :: LocationCard SunkenStairway
sunkenStairway = location SunkenStairway Cards.sunkenStairway 0 (Static 2)

-- TODO: abilities

instance RunMessage SunkenStairway where
  runMessage msg (SunkenStairway attrs) = runQueueT $ SunkenStairway <$> liftRunMessage msg attrs
