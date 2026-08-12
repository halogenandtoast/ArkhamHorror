module Arkham.Homebrew.DarkMatter.Locations.CryosleepQuarters (cryosleepQuarters) where

import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype CryosleepQuarters = CryosleepQuarters LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cryosleepQuarters :: LocationCard CryosleepQuarters
cryosleepQuarters = symbolLabel $ location CryosleepQuarters Cards.cryosleepQuarters 2 (PerPlayer 1)

instance HasAbilities CryosleepQuarters where
  getAbilities (CryosleepQuarters a) = extendRevealed1 a $ locationResignAction a
