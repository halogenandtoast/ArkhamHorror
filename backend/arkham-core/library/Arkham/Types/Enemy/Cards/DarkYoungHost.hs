module Arkham.Types.Enemy.Cards.DarkYoungHost
  ( darkYoungHost
  , DarkYoungHost(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype DarkYoungHost = DarkYoungHost EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkYoungHost :: EnemyCard DarkYoungHost
darkYoungHost = enemy DarkYoungHost Cards.darkYoungHost (4, Static 5, 2) (2, 2)

instance HasModifiersFor env DarkYoungHost

instance ActionRunner env => HasAbilities env DarkYoungHost where
  getAbilities i window (DarkYoungHost attrs) = getAbilities i window attrs

instance (EnemyRunner env) => RunMessage env DarkYoungHost where
  runMessage msg e@(DarkYoungHost attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId -> do
      leadInvestigatorId <- getLeadInvestigatorId
      bayouLocations <- getSetList [Bayou]
      e <$ spawnAtOneOf leadInvestigatorId enemyId bayouLocations
    PlaceClues (LocationTarget lid) n | lid == enemyLocation -> do
      push $ RemoveClues (LocationTarget lid) n
      pure . DarkYoungHost $ attrs & cluesL +~ n
    When (EnemyDefeated eid _ _ _ _ _) | eid == enemyId ->
      e <$ push (PlaceClues (LocationTarget enemyLocation) enemyClues)
    _ -> DarkYoungHost <$> runMessage msg attrs
