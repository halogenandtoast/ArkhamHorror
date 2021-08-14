module Arkham.Types.Enemy.Cards.AlmaHill
  ( AlmaHill(..)
  , almaHill
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Source

newtype AlmaHill = AlmaHill EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

almaHill :: EnemyCard AlmaHill
almaHill = enemyWith
  AlmaHill
  Cards.almaHill
  (3, Static 3, 3)
  (0, 2)
  (spawnAtL ?~ LocationWithTitle "Southside")

instance HasActions AlmaHill where
  getActions (AlmaHill x) =
    restrictedAbility
        x
        1
        OnSameLocation
        (ActionAbility (Just Parley) (ActionCost 1))
      : getActions x

instance (EnemyRunner env) => RunMessage env AlmaHill where
  runMessage msg e@(AlmaHill attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility iid (EnemySource eid) _ 1 _ | eid == enemyId -> e <$ pushAll
      (replicate 3 (InvestigatorDrawEncounterCard iid)
      <> [AddToVictory (toTarget attrs)]
      )
    _ -> AlmaHill <$> runMessage msg attrs
