module Arkham.Enemy.Cards.HuntingNightgaunt (
  huntingNightgaunt,
  HuntingNightgaunt (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype HuntingNightgaunt = HuntingNightgaunt EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

huntingNightgaunt :: EnemyCard HuntingNightgaunt
huntingNightgaunt = enemy HuntingNightgaunt Cards.huntingNightgaunt (3, Static 4, 1) (1, 1)

instance HasModifiersFor HuntingNightgaunt where
  getModifiersFor (HuntingNightgaunt a) =
    getSkillTest >>= \case
      Nothing -> pure mempty
      Just st ->
        fromMaybe mempty <$> runMaybeT do
          liftGuardM $ isEvading a
          modifyEach a (map ChaosTokenTarget st.revealedChaosTokens) [DoubleNegativeModifiersOnChaosTokens]

instance RunMessage HuntingNightgaunt where
  runMessage msg (HuntingNightgaunt attrs) =
    HuntingNightgaunt <$> runMessage msg attrs
