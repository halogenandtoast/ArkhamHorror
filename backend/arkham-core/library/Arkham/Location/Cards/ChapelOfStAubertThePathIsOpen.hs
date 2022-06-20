module Arkham.Location.Cards.ChapelOfStAubertThePathIsOpen
  ( chapelOfStAubertThePathIsOpen
  , ChapelOfStAubertThePathIsOpen(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype ChapelOfStAubertThePathIsOpen = ChapelOfStAubertThePathIsOpen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelOfStAubertThePathIsOpen :: LocationCard ChapelOfStAubertThePathIsOpen
chapelOfStAubertThePathIsOpen = location ChapelOfStAubertThePathIsOpen Cards.chapelOfStAubertThePathIsOpen 3 (PerPlayer 2) Moon [Square]

instance HasAbilities ChapelOfStAubertThePathIsOpen where
  getAbilities (ChapelOfStAubertThePathIsOpen attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage ChapelOfStAubertThePathIsOpen where
  runMessage msg (ChapelOfStAubertThePathIsOpen attrs) =
    ChapelOfStAubertThePathIsOpen <$> runMessage msg attrs
