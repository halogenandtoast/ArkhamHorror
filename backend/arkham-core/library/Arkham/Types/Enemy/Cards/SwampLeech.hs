module Arkham.Types.Enemy.Cards.SwampLeech
  ( SwampLeech(..)
  , swampLeech
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Trait

newtype SwampLeech = SwampLeech EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swampLeech :: EnemyId -> SwampLeech
swampLeech uuid =
  SwampLeech
    $ baseAttrs uuid "81023"
    $ (healthDamageL .~ 1)
    . (fightL .~ 4)
    . (healthL .~ Static 1)
    . (evadeL .~ 0)

instance HasModifiersFor env SwampLeech where
  getModifiersFor = noModifiersFor

isEvade :: Message -> Bool
isEvade = \case
  EvadeEnemy{} -> True
  _ -> False

instance ActionRunner env => HasActions env SwampLeech where
  getActions i window (SwampLeech attrs) = do
    actions' <- getActions i window attrs
    pure $ filter (not . isEvade) actions'

instance EnemyRunner env => RunMessage env SwampLeech where
  runMessage msg e@(SwampLeech attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId -> do
      bayouLocations <- getSetList @LocationId [Bayou]
      e <$ spawnAtOneOf iid enemyId bayouLocations
    EnemyMove eid _ lid | eid == enemyId -> do
      bayouLocations <- getSetList @LocationId [Bayou]
      e <$ when
        (lid `notElem` bayouLocations)
        (unshiftMessage $ Discard (EnemyTarget enemyId))
    _ -> SwampLeech <$> runMessage msg attrs
