module Arkham.Types.Enemy.Cards.AlmaHill
  ( AlmaHill(..)
  , almaHill
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message

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

instance HasAbilities AlmaHill where
  getAbilities (AlmaHill attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 OnSameLocation
        $ ActionAbility (Just Parley) (ActionCost 1)
    ]

instance EnemyRunner env => RunMessage env AlmaHill where
  runMessage msg e@(AlmaHill attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> e <$ pushAll
      (replicate 3 (InvestigatorDrawEncounterCard iid)
      <> [AddToVictory (toTarget attrs)]
      )
    _ -> AlmaHill <$> runMessage msg attrs
