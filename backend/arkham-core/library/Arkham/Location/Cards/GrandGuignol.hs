module Arkham.Location.Cards.GrandGuignol
  ( grandGuignol
  , GrandGuignol(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Attrs

newtype GrandGuignol = GrandGuignol LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandGuignol :: LocationCard GrandGuignol
grandGuignol = location
  GrandGuignol
  Cards.grandGuignol
  3
  (PerPlayer 1)
  Triangle
  [Diamond, Square]

instance HasAbilities GrandGuignol where
  getAbilities (GrandGuignol attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env GrandGuignol where
  runMessage msg (GrandGuignol attrs) = GrandGuignol <$> runMessage msg attrs
