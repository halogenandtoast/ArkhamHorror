module Arkham.Location.Cards.EmergencyRoom (
  emergencyRoom,
  EmergencyRoom (..),
)
where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.SkillTest.Base

newtype EmergencyRoom = EmergencyRoom LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

emergencyRoom :: LocationCard EmergencyRoom
emergencyRoom = location EmergencyRoom Cards.emergencyRoom 2 (PerPlayer 1)

instance HasModifiersFor EmergencyRoom where
  getModifiersFor SkillTestTarget (EmergencyRoom attrs) = do
    mSkillTest <- getSkillTest
    case mSkillTest of
      Just st -> do
        here <- skillTestInvestigator st <=~> investigatorAt (toId attrs)
        pure $ toModifiers attrs [Difficulty (length $ skillTestCommittedCards st) | here]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage EmergencyRoom where
  runMessage msg (EmergencyRoom attrs) =
    EmergencyRoom <$> runMessage msg attrs
