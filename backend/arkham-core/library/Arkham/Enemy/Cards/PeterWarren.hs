module Arkham.Enemy.Cards.PeterWarren (
  PeterWarren (..),
  peterWarren,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action hiding (Ability)
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Message

newtype PeterWarren = PeterWarren EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterWarren :: EnemyCard PeterWarren
peterWarren =
  enemyWith
    PeterWarren
    Cards.peterWarren
    (2, Static 3, 3)
    (1, 0)
    (spawnAtL ?~ "Miskatonic University")

instance HasAbilities PeterWarren where
  getAbilities (PeterWarren attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 OnSameLocation
            $ ActionAbility (Just Parley)
            $ Costs [ActionCost 1, ClueCost (Static 2)]
        ]

instance RunMessage PeterWarren where
  runMessage msg e@(PeterWarren attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AddToVictory $ toTarget attrs
      pure e
    _ -> PeterWarren <$> runMessage msg attrs
