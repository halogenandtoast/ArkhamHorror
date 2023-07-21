module Arkham.Enemy.Cards.VictoriaDevereux (
  VictoriaDevereux (..),
  victoriaDevereux,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action hiding (Ability)
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message

newtype VictoriaDevereux = VictoriaDevereux EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

victoriaDevereux :: EnemyCard VictoriaDevereux
victoriaDevereux =
  enemyWith
    VictoriaDevereux
    Cards.victoriaDevereux
    (3, Static 3, 2)
    (1, 0)
    (spawnAtL ?~ SpawnLocation (LocationWithTitle "Northside"))

instance HasAbilities VictoriaDevereux where
  getAbilities (VictoriaDevereux attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 OnSameLocation $
          ActionAbility (Just Parley) (Costs [ActionCost 1, ResourceCost 5])
      ]

instance RunMessage VictoriaDevereux where
  runMessage msg e@(VictoriaDevereux attrs) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          e <$ push (AddToVictory $ toTarget attrs)
    _ -> VictoriaDevereux <$> runMessage msg attrs
