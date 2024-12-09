module Arkham.Location.Cards.EmergencyRoom (emergencyRoom, EmergencyRoom (..)) where

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.SkillTest.Base

newtype EmergencyRoom = EmergencyRoom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

emergencyRoom :: LocationCard EmergencyRoom
emergencyRoom = location EmergencyRoom Cards.emergencyRoom 2 (PerPlayer 1)

instance HasModifiersFor EmergencyRoom where
  getModifiersFor (EmergencyRoom a) = do
    getSkillTest >>= \case
      Just st -> maybeModified_ a (SkillTestTarget st.id) do
        liftGuardM $ st.investigator <=~> investigatorAt (toId a)
        pure [Difficulty (length $ skillTestCommittedCards st)]
      _ -> pure mempty

instance RunMessage EmergencyRoom where
  runMessage msg (EmergencyRoom attrs) = EmergencyRoom <$> runMessage msg attrs
