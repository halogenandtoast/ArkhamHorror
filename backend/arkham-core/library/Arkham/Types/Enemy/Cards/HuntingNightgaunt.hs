module Arkham.Types.Enemy.Cards.HuntingNightgaunt where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Target

newtype HuntingNightgaunt = HuntingNightgaunt EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingNightgaunt :: EnemyCard HuntingNightgaunt
huntingNightgaunt = enemy HuntingNightgaunt Cards.huntingNightgaunt
  $ (healthDamageL .~ 1)
  . (sanityDamageL .~ 1)
  . (fightL .~ 3)
  . (healthL .~ Static 4)
  . (evadeL .~ 1)

instance HasModifiersFor env HuntingNightgaunt where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env HuntingNightgaunt where
  getActions i window (HuntingNightgaunt attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env HuntingNightgaunt where
  runMessage msg (HuntingNightgaunt attrs@EnemyAttrs {..}) = case msg of
    WhenEvadeEnemy _ eid | eid == enemyId -> do
      unshiftMessage
        (CreateEffect "01172" Nothing (toSource attrs) SkillTestTarget)
      HuntingNightgaunt <$> runMessage msg attrs
    _ -> HuntingNightgaunt <$> runMessage msg attrs
