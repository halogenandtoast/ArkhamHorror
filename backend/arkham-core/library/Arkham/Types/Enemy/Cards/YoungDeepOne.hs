{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.YoungDeepOne where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.SkillType
import Arkham.Types.Source
import ClassyPrelude

newtype YoungDeepOne = YoungDeepOne Attrs
  deriving newtype (Show, ToJSON, FromJSON)

youngDeepOne :: EnemyId -> YoungDeepOne
youngDeepOne uuid = YoungDeepOne $ (baseAttrs uuid "01181")
  { enemyHealthDamage = 1
  , enemySanityDamage = 1
  , enemyFight = 3
  , enemyHealth = Static 3
  , enemyEvade = 3
  , enemyPrey = LowestSkill SkillCombat
  }

instance (IsInvestigator investigator) => HasActions env investigator YoungDeepOne where
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
