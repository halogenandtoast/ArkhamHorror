module Arkham.Enemy.Cards.AlmaHill (
  AlmaHill (..),
  almaHill,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype AlmaHill = AlmaHill EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

almaHill :: EnemyCard AlmaHill
almaHill =
  enemyWith
    AlmaHill
    Cards.almaHill
    (3, Static 3, 3)
    (0, 2)
    (spawnAtL ?~ SpawnLocation (LocationWithTitle "Southside"))

instance HasAbilities AlmaHill where
  getAbilities (AlmaHill attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 OnSameLocation
          $ ActionAbility (Just Parley) (ActionCost 1)
      ]

instance RunMessage AlmaHill where
  runMessage msg e@(AlmaHill attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          e
            <$ pushAll
              ( replicate 3 (InvestigatorDrawEncounterCard iid)
                  <> [AddToVictory (toTarget attrs)]
              )
    _ -> AlmaHill <$> runMessage msg attrs
