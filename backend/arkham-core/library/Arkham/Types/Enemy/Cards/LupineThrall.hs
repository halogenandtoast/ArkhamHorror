module Arkham.Types.Enemy.Cards.LupineThrall
  ( LupineThrall(..)
  , lupineThrall
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.SkillType

newtype LupineThrall = LupineThrall EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lupineThrall :: EnemyCard LupineThrall
lupineThrall = enemy LupineThrall Cards.lupineThrall
  $ (healthDamageL .~ 1)
  . (sanityDamageL .~ 1)
  . (fightL .~ 4)
  . (healthL .~ Static 3)
  . (evadeL .~ 4)
  . (preyL .~ LowestSkill SkillAgility)

instance ActionRunner env => HasActions env LupineThrall where
  getActions i window (LupineThrall attrs) = getActions i window attrs

instance HasModifiersFor env LupineThrall where
  getModifiersFor = noModifiersFor

instance EnemyRunner env => RunMessage env LupineThrall where
  runMessage msg e@(LupineThrall attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId -> do
      farthestLocations <- map unFarthestLocationId <$> getSetList iid
      e <$ spawnAtOneOf iid eid farthestLocations
    _ -> LupineThrall <$> runMessage msg attrs
