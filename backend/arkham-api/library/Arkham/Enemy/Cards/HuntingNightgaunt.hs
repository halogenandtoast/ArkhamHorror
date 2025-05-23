module Arkham.Enemy.Cards.HuntingNightgaunt (huntingNightgaunt) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, isEvading)

newtype HuntingNightgaunt = HuntingNightgaunt EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

huntingNightgaunt :: EnemyCard HuntingNightgaunt
huntingNightgaunt = enemy HuntingNightgaunt Cards.huntingNightgaunt (3, Static 4, 1) (1, 1)

instance HasModifiersFor HuntingNightgaunt where
  getModifiersFor (HuntingNightgaunt a) =
    getSkillTest >>= traverse_ \st -> do
      fromMaybe mempty <$> runMaybeT do
        liftGuardM $ isEvading a
        modifyEach a (map ChaosTokenTarget st.revealedChaosTokens) [DoubleNegativeModifiersOnChaosTokens]

instance RunMessage HuntingNightgaunt where
  runMessage msg (HuntingNightgaunt attrs) =
    HuntingNightgaunt <$> runMessage msg attrs
