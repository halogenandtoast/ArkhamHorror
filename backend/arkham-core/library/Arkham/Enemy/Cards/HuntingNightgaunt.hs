module Arkham.Enemy.Cards.HuntingNightgaunt
  ( huntingNightgaunt
  , HuntingNightgaunt(..)
  ) where

import Arkham.Prelude

import Arkham.Action
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.SkillTest
import Arkham.Source
import Arkham.Target

newtype HuntingNightgaunt = HuntingNightgaunt EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

huntingNightgaunt :: EnemyCard HuntingNightgaunt
huntingNightgaunt =
  enemy HuntingNightgaunt Cards.huntingNightgaunt (3, Static 4, 1) (1, 1)

instance HasModifiersFor HuntingNightgaunt where
  getModifiersFor (TokenTarget _) (HuntingNightgaunt a) = do
    mtarget <- getSkillTestTarget
    mSkillTestSource <- getSkillTestSource
    case (mSkillTestSource, mtarget) of
      (Just (SkillTestSource _ _ _ (Just Evade)), Just target)
        | isTarget a target -> pure
        $ toModifiers a [DoubleNegativeModifiersOnTokens]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage HuntingNightgaunt where
  runMessage msg (HuntingNightgaunt attrs) =
    HuntingNightgaunt <$> runMessage msg attrs
