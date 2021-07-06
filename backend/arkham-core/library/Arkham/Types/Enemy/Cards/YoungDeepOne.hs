module Arkham.Types.Enemy.Cards.YoungDeepOne
  ( YoungDeepOne(..)
  , youngDeepOne
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.SkillType
import Arkham.Types.Source

newtype YoungDeepOne = YoungDeepOne EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youngDeepOne :: EnemyCard YoungDeepOne
youngDeepOne = enemyWith
  YoungDeepOne
  Cards.youngDeepOne
  (3, Static 3, 3)
  (1, 1)
  (preyL .~ LowestSkill SkillCombat)

instance HasModifiersFor env YoungDeepOne where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env YoungDeepOne where
  getActions i window (YoungDeepOne attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env YoungDeepOne where
  runMessage msg (YoungDeepOne attrs@EnemyAttrs {..}) = case msg of
    EnemyEngageInvestigator eid iid | eid == enemyId -> do
      push (InvestigatorAssignDamage iid (EnemySource eid) DamageAny 0 1)
      YoungDeepOne <$> runMessage msg attrs
    EngageEnemy iid eid False | eid == enemyId -> do
      push (InvestigatorAssignDamage iid (EnemySource eid) DamageAny 0 1)
      YoungDeepOne <$> runMessage msg attrs
    _ -> YoungDeepOne <$> runMessage msg attrs
