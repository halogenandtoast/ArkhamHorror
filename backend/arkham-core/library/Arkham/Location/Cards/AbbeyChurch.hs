module Arkham.Location.Cards.AbbeyChurch
  ( abbeyChurch
  , AbbeyChurch(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype AbbeyChurch = AbbeyChurch LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abbeyChurch :: LocationCard AbbeyChurch
abbeyChurch = location
  AbbeyChurch
  Cards.abbeyChurch
  3
  (PerPlayer 1)
  Square
  [Equals, T, Heart, Hourglass, Moon]

instance HasAbilities AbbeyChurch where
  getAbilities (AbbeyChurch attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage AbbeyChurch where
  runMessage msg (AbbeyChurch attrs) = AbbeyChurch <$> runMessage msg attrs
