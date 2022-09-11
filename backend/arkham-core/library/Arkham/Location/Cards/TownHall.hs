module Arkham.Location.Cards.TownHall
  ( townHall
  , TownHall(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TownHall = TownHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

townHall :: LocationCard TownHall
townHall = location TownHall Cards.townHall 4 (PerPlayer 1)

instance HasAbilities TownHall where
  getAbilities (TownHall attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage TownHall where
  runMessage msg (TownHall attrs) = TownHall <$> runMessage msg attrs
