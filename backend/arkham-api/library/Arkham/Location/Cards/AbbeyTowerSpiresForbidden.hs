module Arkham.Location.Cards.AbbeyTowerSpiresForbidden (
  abbeyTowerSpiresForbidden,
  AbbeyTowerSpiresForbidden (..),
) where

import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.ScenarioLogKey

newtype AbbeyTowerSpiresForbidden = AbbeyTowerSpiresForbidden LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

abbeyTowerSpiresForbidden :: LocationCard AbbeyTowerSpiresForbidden
abbeyTowerSpiresForbidden =
  location AbbeyTowerSpiresForbidden Cards.abbeyTowerSpiresForbidden 2 (PerPlayer 3)

instance HasModifiersFor AbbeyTowerSpiresForbidden where
  getModifiersFor (AbbeyTowerSpiresForbidden a) = whenUnrevealed a $ maybeModifySelf a do
    liftGuardM $ not <$> remembered FoundTheTowerKey
    pure [Blocked]

instance RunMessage AbbeyTowerSpiresForbidden where
  runMessage msg (AbbeyTowerSpiresForbidden attrs) =
    AbbeyTowerSpiresForbidden <$> runMessage msg attrs
