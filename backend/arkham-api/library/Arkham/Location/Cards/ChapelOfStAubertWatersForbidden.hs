module Arkham.Location.Cards.ChapelOfStAubertWatersForbidden (
  chapelOfStAubertWatersForbidden,
  ChapelOfStAubertWatersForbidden (..),
) where

import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.ScenarioLogKey

newtype ChapelOfStAubertWatersForbidden = ChapelOfStAubertWatersForbidden LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

chapelOfStAubertWatersForbidden :: LocationCard ChapelOfStAubertWatersForbidden
chapelOfStAubertWatersForbidden =
  location ChapelOfStAubertWatersForbidden Cards.chapelOfStAubertWatersForbidden 2 (PerPlayer 3)

instance HasModifiersFor ChapelOfStAubertWatersForbidden where
  getModifiersFor (ChapelOfStAubertWatersForbidden a) =
    whenUnrevealed a $ blockedUnless a $ remembered FoundAGuide

instance RunMessage ChapelOfStAubertWatersForbidden where
  runMessage msg (ChapelOfStAubertWatersForbidden attrs) =
    ChapelOfStAubertWatersForbidden <$> runMessage msg attrs
