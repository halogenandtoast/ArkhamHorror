module Arkham.Types.Enemy.Cards.HuntingNightgaunt where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype HuntingNightgaunt = HuntingNightgaunt EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingNightgaunt :: EnemyId -> HuntingNightgaunt
huntingNightgaunt uuid =
  HuntingNightgaunt
    $ baseAttrs uuid "01172"
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
