module Arkham.Location.Cards.ChapelOfStAubertWatersForbidden
  ( chapelOfStAubertWatersForbidden
  , ChapelOfStAubertWatersForbidden(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype ChapelOfStAubertWatersForbidden = ChapelOfStAubertWatersForbidden LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelOfStAubertWatersForbidden :: LocationCard ChapelOfStAubertWatersForbidden
chapelOfStAubertWatersForbidden = location ChapelOfStAubertWatersForbidden Cards.chapelOfStAubertWatersForbidden 2 (PerPlayer 3) Moon [Square]

instance HasAbilities ChapelOfStAubertWatersForbidden where
  getAbilities (ChapelOfStAubertWatersForbidden attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage ChapelOfStAubertWatersForbidden where
  runMessage msg (ChapelOfStAubertWatersForbidden attrs) =
    ChapelOfStAubertWatersForbidden <$> runMessage msg attrs
