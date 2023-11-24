module Arkham.Location.Cards.EmergencyRoom
  ( emergencyRoom
  , EmergencyRoom(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype EmergencyRoom = EmergencyRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyRoom :: LocationCard EmergencyRoom
emergencyRoom = location EmergencyRoom Cards.emergencyRoom 2 (PerPlayer 1)

instance HasAbilities EmergencyRoom where
  getAbilities (EmergencyRoom attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage EmergencyRoom where
  runMessage msg (EmergencyRoom attrs) =
    EmergencyRoom <$> runMessage msg attrs
