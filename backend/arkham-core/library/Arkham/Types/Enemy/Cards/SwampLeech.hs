module Arkham.Types.Enemy.Cards.SwampLeech
  ( SwampLeech(..)
  , swampLeech
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype SwampLeech = SwampLeech EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swampLeech :: EnemyCard SwampLeech
swampLeech = enemy SwampLeech Cards.swampLeech (4, Static 1, 0) (1, 0)

instance HasModifiersFor env SwampLeech

isEvade :: Ability -> Bool
isEvade ability = case abilityType ability of
  ActionAbility (Just Action.Evade) _ -> True
  _ -> False

instance ActionRunner env => HasAbilities env SwampLeech where
  getAbilities i window (SwampLeech attrs) = do
    actions' <- getAbilities i window attrs
    pure $ filter (not . isEvade) actions'

instance EnemyRunner env => RunMessage env SwampLeech where
  runMessage msg e@(SwampLeech attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId -> do
      bayouLocations <- getSetList @LocationId [Bayou]
      e <$ spawnAtOneOf iid enemyId bayouLocations
    EnemyEntered eid lid | eid == enemyId -> do
      bayouLocations <- getSetList @LocationId [Bayou]
      e <$ when
        (lid `notElem` bayouLocations)
        (push $ Discard (EnemyTarget enemyId))
    _ -> SwampLeech <$> runMessage msg attrs
