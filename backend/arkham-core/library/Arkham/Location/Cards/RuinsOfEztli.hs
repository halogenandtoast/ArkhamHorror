module Arkham.Location.Cards.RuinsOfEztli
  ( ruinsOfEztli
  , RuinsOfEztli(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype RuinsOfEztli = RuinsOfEztli LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfEztli :: LocationCard RuinsOfEztli
ruinsOfEztli = location
  RuinsOfEztli
  Cards.ruinsOfEztli
  3
  (PerPlayer 2)
  Hourglass
  [Triangle, Heart]

instance HasAbilities RuinsOfEztli where
  getAbilities (RuinsOfEztli attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage RuinsOfEztli where
  runMessage msg (RuinsOfEztli attrs) = RuinsOfEztli <$> runMessage msg attrs
