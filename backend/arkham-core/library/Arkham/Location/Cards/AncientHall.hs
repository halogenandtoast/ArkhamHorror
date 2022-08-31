module Arkham.Location.Cards.AncientHall
  ( ancientHall
  , AncientHall(..)
  ) where

import Arkham.Prelude

import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype AncientHall = AncientHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientHall :: LocationCard AncientHall
ancientHall = locationWith
  AncientHall
  Cards.ancientHall
  3
  (PerPlayer 2)
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasAbilities AncientHall where
  getAbilities (AncientHall attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage AncientHall where
  runMessage msg (AncientHall attrs) = AncientHall <$> runMessage msg attrs
