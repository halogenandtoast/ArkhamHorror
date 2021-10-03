module Arkham.Types.Location.Cards.BasementHall
  ( basementHall
  , BasementHall(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype BasementHall = BasementHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

basementHall :: LocationCard BasementHall
basementHall = location
  BasementHall
  Cards.basementHall
  4
  (PerPlayer 1)
  Squiggle
  [Hourglass, Moon]

instance HasAbilities BasementHall where
  getAbilities (BasementHall attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env BasementHall where
  runMessage msg (BasementHall attrs) = BasementHall <$> runMessage msg attrs
