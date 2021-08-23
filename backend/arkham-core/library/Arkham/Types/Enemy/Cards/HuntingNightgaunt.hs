module Arkham.Types.Enemy.Cards.HuntingNightgaunt
  ( huntingNightgaunt
  , HuntingNightgaunt(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message
import Arkham.Types.Target

newtype HuntingNightgaunt = HuntingNightgaunt EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities env)

huntingNightgaunt :: EnemyCard HuntingNightgaunt
huntingNightgaunt =
  enemy HuntingNightgaunt Cards.huntingNightgaunt (3, Static 4, 1) (1, 1)

instance EnemyRunner env => RunMessage env HuntingNightgaunt where
  runMessage msg (HuntingNightgaunt attrs@EnemyAttrs {..}) = case msg of
    WhenEvadeEnemy _ eid | eid == enemyId -> do
      push (CreateEffect "01172" Nothing (toSource attrs) SkillTestTarget)
      HuntingNightgaunt <$> runMessage msg attrs
    _ -> HuntingNightgaunt <$> runMessage msg attrs
