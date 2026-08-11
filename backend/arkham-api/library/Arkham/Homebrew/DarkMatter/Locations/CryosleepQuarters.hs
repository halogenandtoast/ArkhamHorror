module Arkham.Homebrew.DarkMatter.Locations.CryosleepQuarters (cryosleepQuarters) where

import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype CryosleepQuarters = CryosleepQuarters LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cryosleepQuarters :: LocationCard CryosleepQuarters
cryosleepQuarters = symbolLabel $ location CryosleepQuarters Cards.cryosleepQuarters 2 (PerPlayer 1)

-- "[action]: Resign. Return to your cryo-chamber and hope you wake up from this nightmare."
instance HasAbilities CryosleepQuarters where
  getAbilities (CryosleepQuarters a) = extendRevealed1 a $ locationResignAction a

instance RunMessage CryosleepQuarters where
  runMessage msg (CryosleepQuarters attrs) = CryosleepQuarters <$> runMessage msg attrs
