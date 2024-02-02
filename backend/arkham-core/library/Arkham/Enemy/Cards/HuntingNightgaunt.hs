module Arkham.Enemy.Cards.HuntingNightgaunt (
  huntingNightgaunt,
  HuntingNightgaunt (..),
) where

import Arkham.Prelude

import Arkham.Action
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype HuntingNightgaunt = HuntingNightgaunt EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

huntingNightgaunt :: EnemyCard HuntingNightgaunt
huntingNightgaunt = enemy HuntingNightgaunt Cards.huntingNightgaunt (3, Static 4, 1) (1, 1)

instance HasModifiersFor HuntingNightgaunt where
  getModifiersFor (ChaosTokenTarget _) (HuntingNightgaunt a) = do
    mTarget <- getSkillTestTarget
    mAction <- getSkillTestAction
    case (mAction, mTarget) of
      (Just Evade, Just (isTarget a -> True)) -> do
        pure $ toModifiers a [DoubleNegativeModifiersOnChaosTokens]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage HuntingNightgaunt where
  runMessage msg (HuntingNightgaunt attrs) =
    HuntingNightgaunt <$> runMessage msg attrs
