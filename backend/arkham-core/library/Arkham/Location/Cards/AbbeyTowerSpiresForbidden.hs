module Arkham.Location.Cards.AbbeyTowerSpiresForbidden (
  abbeyTowerSpiresForbidden,
  AbbeyTowerSpiresForbidden (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.ScenarioLogKey

newtype AbbeyTowerSpiresForbidden = AbbeyTowerSpiresForbidden LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

abbeyTowerSpiresForbidden :: LocationCard AbbeyTowerSpiresForbidden
abbeyTowerSpiresForbidden =
  location
    AbbeyTowerSpiresForbidden
    Cards.abbeyTowerSpiresForbidden
    2
    (PerPlayer 3)

instance HasModifiersFor AbbeyTowerSpiresForbidden where
  getModifiersFor target (AbbeyTowerSpiresForbidden attrs)
    | isTarget attrs target = do
        foundAGuide <- remembered FoundTheTowerKey
        pure
          $ toModifiers
            attrs
            [Blocked | not (locationRevealed attrs) && not foundAGuide]
  getModifiersFor _ _ = pure []

instance RunMessage AbbeyTowerSpiresForbidden where
  runMessage msg (AbbeyTowerSpiresForbidden attrs) =
    AbbeyTowerSpiresForbidden <$> runMessage msg attrs
