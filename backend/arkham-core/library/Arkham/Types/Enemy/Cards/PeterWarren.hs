module Arkham.Types.Enemy.Cards.PeterWarren
  ( PeterWarren(..)
  , peterWarren
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action hiding (Ability)
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message

newtype PeterWarren = PeterWarren EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
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

instance EnemyRunner env => RunMessage env PeterWarren where
  runMessage msg e@(PeterWarren attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (AddToVictory $ toTarget attrs)
    _ -> PeterWarren <$> runMessage msg attrs
