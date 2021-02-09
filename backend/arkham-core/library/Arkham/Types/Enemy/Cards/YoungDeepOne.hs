module Arkham.Types.Enemy.Cards.YoungDeepOne where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype YoungDeepOne = YoungDeepOne EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youngDeepOne :: EnemyId -> YoungDeepOne
youngDeepOne uuid =
  YoungDeepOne
    $ baseAttrs uuid "01181"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 3)
    . (healthL .~ Static 3)
    . (evadeL .~ 3)
    . (preyL .~ LowestSkill SkillCombat)

instance HasModifiersFor env YoungDeepOne where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env YoungDeepOne where
  getActions i window (YoungDeepOne attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env YoungDeepOne where
  runMessage msg (YoungDeepOne attrs@EnemyAttrs {..}) = case msg of
    EnemyEngageInvestigator eid iid | eid == enemyId -> do
      unshiftMessage (InvestigatorAssignDamage iid (EnemySource eid) DamageAny 0 1)
      YoungDeepOne <$> runMessage msg attrs
    EngageEnemy iid eid False | eid == enemyId -> do
      unshiftMessage (InvestigatorAssignDamage iid (EnemySource eid) DamageAny 0 1)
      YoungDeepOne <$> runMessage msg attrs
    _ -> YoungDeepOne <$> runMessage msg attrs
