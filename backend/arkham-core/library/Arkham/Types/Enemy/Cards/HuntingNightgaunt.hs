module Arkham.Types.Enemy.Cards.HuntingNightgaunt
  ( huntingNightgaunt
  , HuntingNightgaunt(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target

newtype HuntingNightgaunt = HuntingNightgaunt EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

huntingNightgaunt :: EnemyCard HuntingNightgaunt
huntingNightgaunt =
  enemy HuntingNightgaunt Cards.huntingNightgaunt (3, Static 4, 1) (1, 1)

instance HasModifiersFor env HuntingNightgaunt where
  getModifiersFor (SkillTestSource _ _ _ target (Just Evade)) (TokenTarget _) (HuntingNightgaunt a)
    | isTarget a target
    = pure $ toModifiers a [DoubleNegativeModifiersOnTokens]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env HuntingNightgaunt where
  runMessage msg (HuntingNightgaunt attrs) =
    HuntingNightgaunt <$> runMessage msg attrs
