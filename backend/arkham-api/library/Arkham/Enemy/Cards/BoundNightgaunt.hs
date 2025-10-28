module Arkham.Enemy.Cards.BoundNightgaunt (boundNightgaunt) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, isFighting)

newtype BoundNightgaunt = BoundNightgaunt EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

boundNightgaunt :: EnemyCard BoundNightgaunt
boundNightgaunt = enemy BoundNightgaunt Cards.boundNightgaunt (1, Static 3, 3) (0, 2)

instance HasModifiersFor BoundNightgaunt where
  getModifiersFor (BoundNightgaunt a) =
    getSkillTest >>= traverse_ \st -> do
      fromMaybe mempty <$> runMaybeT do
        liftGuardM $ isFighting a
        modifyEach a (map ChaosTokenTarget st.revealedChaosTokens) [DoubleNegativeModifiersOnChaosTokens]

instance RunMessage BoundNightgaunt where
  runMessage msg (BoundNightgaunt attrs) = runQueueT $ case msg of
    _ -> BoundNightgaunt <$> liftRunMessage msg attrs
