module Arkham.Types.Enemy.Cards.MarshGug
  ( marshGug
  , MarshGug(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message
import Arkham.Types.Trait

newtype MarshGug = MarshGug EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marshGug :: EnemyCard MarshGug
marshGug = enemy MarshGug Cards.marshGug (3, Static 4, 3) (2, 1)

instance HasModifiersFor env MarshGug

instance ActionRunner env => HasAbilities env MarshGug where
  getAbilities i window (MarshGug attrs) = getAbilities i window attrs

instance (EnemyRunner env) => RunMessage env MarshGug where
  runMessage msg e@(MarshGug attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId -> do
      leadInvestigatorId <- getLeadInvestigatorId
      bayouLocations <- getSetList [Bayou]
      e <$ spawnAtOneOf leadInvestigatorId enemyId bayouLocations
    _ -> MarshGug <$> runMessage msg attrs
