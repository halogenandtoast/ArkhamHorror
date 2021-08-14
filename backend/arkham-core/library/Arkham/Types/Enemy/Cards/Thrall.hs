module Arkham.Types.Enemy.Cards.Thrall
  ( Thrall(..)
  , thrall
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Exception
import Arkham.Types.Message
import Arkham.Types.Query

newtype Thrall = Thrall EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thrall :: EnemyCard Thrall
thrall = enemy Thrall Cards.thrall (2, Static 2, 2) (1, 1)

instance HasModifiersFor env Thrall

instance ActionRunner env => HasAbilities env Thrall where
  getAbilities i window (Thrall attrs) = getAbilities i window attrs

instance EnemyRunner env => RunMessage env Thrall where
  runMessage msg e@(Thrall attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId -> do
      locations <- getSetList ()
        >>= traverse (traverseToSnd $ (unClueCount <$>) . getCount)
      case maxes locations of
        [] -> throwIO (InvalidState "No locations")
        xs -> e <$ spawnAtOneOf iid enemyId xs
    _ -> Thrall <$> runMessage msg attrs
