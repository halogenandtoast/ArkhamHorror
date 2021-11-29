module Arkham.Types.Location.Cards.Montparnasse
  ( montparnasse
  , Montparnasse(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype Montparnasse = Montparnasse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

montparnasse :: LocationCard Montparnasse
montparnasse = location
  Montparnasse
  Cards.montparnasse
  2
  (PerPlayer 1)
  Circle
  [Heart, Star, Plus]

instance HasAbilities Montparnasse where
  getAbilities (Montparnasse attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env Montparnasse where
  runMessage msg (Montparnasse attrs) = Montparnasse <$> runMessage msg attrs
