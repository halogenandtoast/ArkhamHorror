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
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Source
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype AlmaHill = AlmaHill EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

almaHill :: EnemyCard AlmaHill
almaHill = enemyWith
  AlmaHill
  Cards.almaHill
  (3, Static 3, 3)
  (0, 2)
  (spawnAtL ?~ LocationWithTitle "Southside")

instance HasModifiersFor env AlmaHill

instance ActionRunner env => HasAbilities env AlmaHill where
  getAbilities iid window@(Window Timing.When NonFast) (AlmaHill attrs@EnemyAttrs {..})
    = withBaseAbilities iid window attrs $ do
      locationId <- getId @LocationId iid
      pure
        [ mkAbility attrs 1 $ ActionAbility (Just Parley) (ActionCost 1)
        | locationId == enemyLocation
        ]
  getAbilities _ _ _ = pure []

instance (EnemyRunner env) => RunMessage env AlmaHill where
  runMessage msg e@(AlmaHill attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility iid (EnemySource eid) _ 1 _ | eid == enemyId -> e <$ pushAll
      (replicate 3 (InvestigatorDrawEncounterCard iid)
      <> [AddToVictory (toTarget attrs)]
      )
    _ -> AlmaHill <$> runMessage msg attrs
