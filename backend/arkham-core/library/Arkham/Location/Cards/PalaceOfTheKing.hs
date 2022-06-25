module Arkham.Location.Cards.PalaceOfTheKing
  ( palaceOfTheKing
  , PalaceOfTheKing(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype PalaceOfTheKing = PalaceOfTheKing LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

palaceOfTheKing :: LocationCard PalaceOfTheKing
palaceOfTheKing = location
  PalaceOfTheKing
  Cards.palaceOfTheKing
  2
  (PerPlayer 3)
  Star
  [Triangle, Diamond]

instance HasAbilities PalaceOfTheKing where
  getAbilities (PalaceOfTheKing attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage PalaceOfTheKing where
  runMessage msg (PalaceOfTheKing attrs) =
    PalaceOfTheKing <$> runMessage msg attrs
