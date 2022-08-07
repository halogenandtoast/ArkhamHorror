module Arkham.Enemy.Cards.PeterWarren
  ( PeterWarren(..)
  , peterWarren
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Action hiding (Ability)
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message

newtype PeterWarren = PeterWarren EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterWarren :: EnemyCard PeterWarren
peterWarren = enemyWith
  PeterWarren
  Cards.peterWarren
  (2, Static 3, 3)
  (1, 0)
  (spawnAtL ?~ LocationWithTitle "Miskatonic University")

instance HasAbilities PeterWarren where
  getAbilities (PeterWarren attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 OnSameLocation
        $ ActionAbility (Just Parley) (Costs [ActionCost 1, ClueCost 2])
    ]

instance RunMessage PeterWarren where
  runMessage msg e@(PeterWarren attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (AddToVictory $ toTarget attrs)
    _ -> PeterWarren <$> runMessage msg attrs
