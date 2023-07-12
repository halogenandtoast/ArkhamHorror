module Arkham.Location.Cards.TheGeistTrap
  ( theGeistTrap
  , TheGeistTrap(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TheGeistTrap = TheGeistTrap LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGeistTrap :: LocationCard TheGeistTrap
theGeistTrap = location TheGeistTrap Cards.theGeistTrap 4 (PerPlayer 1)

instance HasAbilities TheGeistTrap where
  getAbilities (TheGeistTrap attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage TheGeistTrap where
  runMessage msg (TheGeistTrap attrs) =
    TheGeistTrap <$> runMessage msg attrs
