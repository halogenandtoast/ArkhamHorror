module Arkham.Enemy.Cards.VictoriaDevereux (
  VictoriaDevereux (..),
  victoriaDevereux,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype VictoriaDevereux = VictoriaDevereux EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

victoriaDevereux :: EnemyCard VictoriaDevereux
victoriaDevereux =
  enemyWith VictoriaDevereux Cards.victoriaDevereux (3, Static 3, 2) (1, 0) (spawnAtL ?~ "Northside")

instance HasAbilities VictoriaDevereux where
  getAbilities (VictoriaDevereux attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 OnSameLocation
            $ ActionAbility [Parley]
            $ Costs [ActionCost 1, ResourceCost 5]
        ]

instance RunMessage VictoriaDevereux where
  runMessage msg e@(VictoriaDevereux attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AddToVictory $ toTarget attrs
      pure e
    _ -> VictoriaDevereux <$> runMessage msg attrs
