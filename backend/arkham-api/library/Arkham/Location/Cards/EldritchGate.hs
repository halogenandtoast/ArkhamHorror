module Arkham.Location.Cards.EldritchGate (eldritchGate) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype EldritchGate = EldritchGate LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eldritchGate :: LocationCard EldritchGate
eldritchGate = symbolLabel $ location EldritchGate Cards.eldritchGate 4 (Static 1)

instance RunMessage EldritchGate where
  runMessage msg (EldritchGate attrs) = EldritchGate <$> runMessage msg attrs
