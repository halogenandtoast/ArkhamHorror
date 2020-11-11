{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.YoungDeepOne where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype YoungDeepOne = YoungDeepOne Attrs
  deriving newtype (Show, ToJSON, FromJSON)

youngDeepOne :: EnemyId -> YoungDeepOne
youngDeepOne uuid =
  YoungDeepOne
    $ baseAttrs uuid "01181"
    $ (healthDamage .~ 1)
    . (sanityDamage .~ 1)
    . (fight .~ 3)
    . (health .~ Static 3)
    . (evade .~ 3)
    . (prey .~ LowestSkill SkillCombat)

instance HasModifiersFor env YoungDeepOne where
  getModifiersFor = noModifiersFor

instance HasModifiers env YoungDeepOne where
  getModifiers _ (YoungDeepOne Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env YoungDeepOne where
  getActions i window (YoungDeepOne attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env YoungDeepOne where
  runMessage msg (YoungDeepOne attrs@Attrs {..}) = case msg of
    EnemyEngageInvestigator eid iid | eid == enemyId -> do
      unshiftMessage (InvestigatorAssignDamage iid (EnemySource eid) 0 1)
      YoungDeepOne <$> runMessage msg attrs
    EngageEnemy iid eid False | eid == enemyId -> do
      unshiftMessage (InvestigatorAssignDamage iid (EnemySource eid) 0 1)
      YoungDeepOne <$> runMessage msg attrs
    _ -> YoungDeepOne <$> runMessage msg attrs
